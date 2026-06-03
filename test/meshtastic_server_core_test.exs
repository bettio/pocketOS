defmodule MeshtasticServerCoreTest do
  # Pure unit tests for the functional core. No process, no mock radio, no
  # timers: every test passes an explicit core state + Env and asserts on the
  # returned {Reply, CoreState, Effects} and the resulting tx_queue (read back
  # via take_due/2). The gen_server wiring is covered by meshtastic_server_test.
  use ExUnit.Case, async: true
  import Bitwise

  @us 0xDEADCAFE
  @peer 0xAAAAAAAA
  @broadcast 0xFFFFFFFF
  @orig_pid 0x0BADF00D
  @attrs %{rssi: -28, snr: 11}

  # ---- helpers ----

  defp core(opts \\ []) do
    {core, _effects} = :meshtastic_server_core.init(Keyword.put_new(opts, :node_id, @us), 0)
    core
  end

  defp psk, do: :meshtastic.default_long_fast_psk()

  defp channel_hash do
    %{hash: h} = :meshtastic.default_long_fast_channel()
    h
  end

  defp text_data(text) do
    %{portnum: :TEXT_MESSAGE_APP, payload: text}
    |> :meshtastic_proto.encode()
    |> :erlang.iolist_to_binary()
  end

  # Build a parsed inbound packet (the shape meshtastic:parse/1 hands the core),
  # channel-encrypted with the default PSK.
  defp rx_packet(fields) do
    base = %{
      dest: @broadcast,
      src: @peer,
      packet_id: @orig_pid,
      hop_start: 3,
      via_mqtt: false,
      want_ack: false,
      hop_limit: 3,
      channel_hash: channel_hash(),
      next_hop: 0,
      relay_node: 0xAA,
      data: text_data("hi")
    }

    wire =
      base
      |> Map.merge(fields)
      |> :meshtastic.encrypt(psk())
      |> :meshtastic.serialize()

    {:ok, packet} = :meshtastic.parse(wire)
    packet
  end

  defp env(extra \\ %{}), do: Map.merge(%{now_ms: 1000, rand22: 0x2ABCDE}, extra)

  # Drain the wire payloads currently due for TX. take_due/2 returns full
  # tx_intent maps now, so pull each intent's payload for assertions.
  defp drain(core) do
    {intents, _core2} = :meshtastic_server_core.take_due(core, 1_000_000)
    Enum.map(intents, & &1.payload)
  end

  defp decode_wire(wire) do
    {:ok, p} = :meshtastic.parse(wire)
    msg = p |> :meshtastic.decrypt(psk()) |> Map.fetch!(:data) |> :meshtastic_proto.decode()
    {p, msg}
  end

  # ---- init ----

  test "init/2 applies defaults and arms the first periodic timer" do
    {_core, effects} = :meshtastic_server_core.init([], 12_345)
    assert effects == [{:set_timer, 500, :periodic}]
  end

  test "init/2 default node_id is used when originating" do
    c = elem(:meshtastic_server_core.init([], 0), 0)
    {:ok, c2, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("x"), env(), c)
    [wire] = drain(c2)
    {p, _} = decode_wire(wire)
    assert p.src == 1_127_302_788
  end

  # ---- handle_rx: delivery ----

  test "a channel broadcast is delivered and rebroadcast" do
    packet = rx_packet(%{data: text_data("ciao")})
    {:ok, core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [{:deliver, decoded}] = effects
    assert decoded.dest == @broadcast
    assert decoded.want_ack == false
    assert decoded.rssi == -28
    assert decoded.snr == 11
    assert decoded.message == %{portnum: :TEXT_MESSAGE_APP, payload: "ciao"}

    # broadcast is a recipient AND floodable -> exactly one rebroadcast
    assert [rebroadcast] = drain(core2)
    {p, _} = decode_wire(rebroadcast)
    assert p.packet_id == @orig_pid
  end

  test "a unicast to us is delivered but not rebroadcast" do
    packet = rx_packet(%{dest: @us, data: text_data("dm")})

    {:ok, core2, [{:deliver, decoded}]} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert decoded.message.payload == "dm"
    assert drain(core2) == []
  end

  test "a packet from our own node id is discarded untouched" do
    packet = rx_packet(%{src: @us})
    c = core()
    assert {:discard, ^c, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)
  end

  # ---- handle_rx: dedup ----

  test "a duplicate is discarded with no tx and the dedup state is threaded" do
    packet = rx_packet(%{data: text_data("once")})
    {:ok, core1, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    # drain the first rebroadcast intent so it can't be confused with a second one
    {[_intent], core1b} = :meshtastic_server_core.take_due(core1, 1_000_000)

    {:discard, core2, effects} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{now_ms: 1001}), core1b)

    assert effects == []
    assert drain(core2) == []
  end

  # ---- handle_rx: decrypt / decode failures keep last_seen untouched ----

  test "a decode failure is discarded and last_seen is left untouched" do
    # decrypted bytes are a truncated length-delimited protobuf field -> decode throws
    packet = rx_packet(%{dest: @us, data: <<0x12, 0x05, 0x41>>})
    c = core()
    assert {:discard, ^c, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)
  end

  test "a PKI packet whose peer key did not resolve is discarded untouched" do
    {_pub, priv} = :crypto.generate_key(:eddh, :x25519)
    packet = rx_packet(%{dest: @us, channel_hash: 0})
    c = core(private_key: priv)
    e = env(%{peer_key: {:error, :not_found}})
    assert {:discard, ^c, []} = :meshtastic_server_core.handle_rx(packet, @attrs, e, c)
  end

  # ---- handle_rx: PKI happy path ----

  test "a PKI direct message is decrypted with the pre-resolved peer key" do
    bob = 0xBBBBBBBB
    {alice_pub, alice_priv} = :crypto.generate_key(:eddh, :x25519)
    {bob_pub, bob_priv} = :crypto.generate_key(:eddh, :x25519)

    wire =
      %{
        dest: bob,
        src: @peer,
        packet_id: 0x12345678,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: 0,
        next_hop: 0,
        relay_node: 0,
        data: text_data("hi from alice")
      }
      |> :meshtastic.encrypt_pki(alice_priv, bob_pub)
      |> :meshtastic.serialize()

    {:ok, packet} = :meshtastic.parse(wire)
    c = core(node_id: bob, private_key: bob_priv)
    e = env(%{peer_key: {:ok, alice_pub}})

    {:ok, core2, [{:deliver, decoded}]} =
      :meshtastic_server_core.handle_rx(packet, @attrs, e, c)

    assert decoded.message == %{portnum: :TEXT_MESSAGE_APP, payload: "hi from alice"}
    assert drain(core2) == []
  end

  # ---- rx_needs_peer_key predicate ----

  test "rx_needs_peer_key only fires for PKI unicasts to us with a private key" do
    {_pub, priv} = :crypto.generate_key(:eddh, :x25519)
    with_key = core(private_key: priv)

    assert :meshtastic_server_core.rx_needs_peer_key(
             rx_packet(%{dest: @us, channel_hash: 0}),
             with_key
           )

    refute :meshtastic_server_core.rx_needs_peer_key(
             rx_packet(%{dest: @us, channel_hash: 0}),
             core()
           )

    refute :meshtastic_server_core.rx_needs_peer_key(
             rx_packet(%{dest: @broadcast, channel_hash: 0}),
             with_key
           )

    refute :meshtastic_server_core.rx_needs_peer_key(
             rx_packet(%{dest: @us, channel_hash: channel_hash()}),
             with_key
           )
  end

  # ---- handle_rx: NodeInfo want_response auto-reply ----

  defp user_info do
    %{
      id: "!deadcafe",
      long_name: "pocketOS test",
      short_name: "dc",
      hw_model: 50,
      role: :CLIENT,
      is_licensed: false,
      macaddr: <<0xDE, 0xAD, 0xCA, 0xFE, 0x00, 0x01>>,
      public_key: :binary.copy(<<0>>, 32)
    }
  end

  defp nodeinfo_data(opts) do
    %{
      portnum: :NODEINFO_APP,
      payload: %{id: "!aaaaaaaa", long_name: "peer", short_name: "pe", hw_model: 50}
    }
    |> Map.merge(Map.new(opts))
    |> :meshtastic_proto.encode()
    |> :erlang.iolist_to_binary()
  end

  test "a NodeInfo want_response request gets a unicast reply tagged with its packet id" do
    packet = rx_packet(%{dest: @us, data: nodeinfo_data(want_response: true)})
    c = core(node_info: %{user_info: user_info()})

    {:ok, core2, [{:deliver, decoded}]} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)

    assert decoded.want_response == true

    assert [reply] = drain(core2)
    {p, msg} = decode_wire(reply)
    assert p.dest == @peer
    assert p.src == @us
    assert msg.portnum == :NODEINFO_APP
    assert msg.request_id == @orig_pid
    assert msg.payload.long_name == "pocketOS test"
  end

  test "a NodeInfo request without user_info produces no reply" do
    packet = rx_packet(%{dest: @us, data: nodeinfo_data(want_response: true)})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert drain(core2) == []
  end

  test "a NodeInfo request with want_response=false produces no reply" do
    packet = rx_packet(%{dest: @us, data: nodeinfo_data([])})
    c = core(node_info: %{user_info: user_info()})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)
    assert drain(core2) == []
  end

  # ---- handle_rx: ROUTING ACK ----

  test "a want_ack unicast gets a ROUTING ack tagged with the request id" do
    packet = rx_packet(%{dest: @us, want_ack: true, data: text_data("ping")})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [ack] = drain(core2)
    {p, msg} = decode_wire(ack)
    assert p.dest == @peer
    assert msg.portnum == :ROUTING_APP
    assert msg.request_id == @orig_pid
  end

  test "we do not ack a packet that is itself an ack" do
    routing =
      %{portnum: :ROUTING_APP, payload: %{error_reason: :NONE}, request_id: 0x55}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    packet = rx_packet(%{dest: @us, want_ack: true, data: routing})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert drain(core2) == []
  end

  test "a want_ack broadcast is not acked but is rebroadcast" do
    packet = rx_packet(%{dest: @broadcast, want_ack: true, data: text_data("hey")})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    # the only tx is the rebroadcast (reuses the inbound id), never an ack
    assert [rebroadcast] = drain(core2)
    {p, _} = decode_wire(rebroadcast)
    assert p.packet_id == @orig_pid
  end

  test "a NodeInfo want_response reply takes precedence over the ack" do
    packet = rx_packet(%{dest: @us, want_ack: true, data: nodeinfo_data(want_response: true)})
    c = core(node_info: %{user_info: user_info()})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)

    assert [reply] = drain(core2)
    {_p, msg} = decode_wire(reply)
    assert msg.portnum == :NODEINFO_APP
  end

  # ---- handle_rx: TRACEROUTE want_response auto-reply ----

  defp traceroute_data(payload, opts) do
    %{portnum: :TRACEROUTE_APP, payload: payload}
    |> Map.merge(Map.new(opts))
    |> :meshtastic_proto.encode()
    |> :erlang.iolist_to_binary()
  end

  @traceroute_request <<0x08, 0x46, 0x18, 0x01>>

  test "a unicast traceroute request to us gets a route_reply with our snr appended" do
    packet = rx_packet(%{dest: @us, data: @traceroute_request})

    {:ok, core2, [{:deliver, decoded}]} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert decoded.message.portnum == :TRACEROUTE_APP

    assert [reply] = drain(core2)
    {p, msg} = decode_wire(reply)
    assert p.dest == @peer
    assert p.src == @us
    assert msg.portnum == :TRACEROUTE_APP
    assert msg.request_id == @orig_pid
    assert msg.payload.snr_towards == [44]
    refute Map.has_key?(msg.payload, :route)
  end

  test "an unknown rx snr is encoded as the INT8_MIN sentinel in the reply" do
    packet = rx_packet(%{dest: @us, data: @traceroute_request})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, %{rssi: -50}, env(), core())

    assert [reply] = drain(core2)
    {_p, msg} = decode_wire(reply)
    assert msg.payload.snr_towards == [-128]
  end

  test "a multi-hop traceroute reply preserves the inbound route and appends our snr" do
    data = traceroute_data(%{route: [0xA, 0xB], snr_towards: [20, -8]}, want_response: true)
    packet = rx_packet(%{dest: @us, data: data})

    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [reply] = drain(core2)
    {_p, msg} = decode_wire(reply)
    assert msg.payload.route == [0xA, 0xB]
    assert msg.payload.snr_towards == [20, -8, 44]
  end

  test "a traceroute reply pads hops that upstream relays left unannotated" do
    packet =
      rx_packet(%{
        dest: @us,
        data: traceroute_data(%{}, want_response: true),
        hop_start: 4,
        hop_limit: 2
      })

    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [reply] = drain(core2)
    {_p, msg} = decode_wire(reply)
    assert msg.request_id == @orig_pid
    assert msg.payload.route == [0xFFFFFFFF, 0xFFFFFFFF]
    assert msg.payload.snr_towards == [-128, -128, 44]
  end

  test "a traceroute reply takes precedence over the ROUTING ack" do
    packet =
      rx_packet(%{dest: @us, want_ack: true, data: traceroute_data(%{}, want_response: true)})

    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [reply] = drain(core2)
    {p, msg} = decode_wire(reply)
    assert p.dest == @peer
    assert msg.portnum == :TRACEROUTE_APP
    assert msg.request_id == @orig_pid
  end

  test "a broadcast traceroute request is flooded (annotated) but not replied to" do
    packet = rx_packet(%{dest: @broadcast, data: traceroute_data(%{}, want_response: true)})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [rebroadcast] = drain(core2)
    {p, msg} = decode_wire(rebroadcast)
    assert p.packet_id == @orig_pid
    assert p.hop_limit == 2
    assert msg.payload.route == [@us]
    assert msg.payload.snr_towards == [44]
  end

  test "a traceroute response to us is neither replied to nor acked" do
    data =
      %{portnum: :TRACEROUTE_APP, payload: %{snr_towards: [10]}, request_id: 0x99}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    packet = rx_packet(%{dest: @us, want_ack: true, data: data})
    {:ok, core2, _effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert drain(core2) == []
  end

  # ---- handle_send: TRACEROUTE initiator (the Traceroute app's send path) ----

  defp traceroute_request_data do
    %{portnum: :TRACEROUTE_APP, payload: %{}, want_response: true}
    |> :meshtastic_proto.encode()
    |> :erlang.iolist_to_binary()
  end

  test "handle_send originates a TRACEROUTE_APP unicast carrying want_response" do
    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@peer, traceroute_request_data(), env(), core())

    assert [wire] = drain(core2)
    {p, msg} = decode_wire(wire)
    assert p.dest == @peer
    assert p.src == @us
    assert msg.portnum == :TRACEROUTE_APP
    assert msg.want_response == true
  end

  test "the request we originate is responder-compatible (yields a route_reply)" do
    packet = rx_packet(%{dest: @us, data: traceroute_request_data()})

    {:ok, core2, [{:deliver, decoded} | _]} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert decoded.message.portnum == :TRACEROUTE_APP
    assert decoded.message.want_response == true

    assert [reply] = drain(core2)
    {p, msg} = decode_wire(reply)
    assert p.dest == @peer
    assert msg.portnum == :TRACEROUTE_APP
    assert msg.request_id == @orig_pid
    assert msg.payload.snr_towards == [44]
  end

  test "a traceroute reply to us is delivered with route + request_id for the report" do
    data =
      %{
        portnum: :TRACEROUTE_APP,
        payload: %{route: [0xA, 0xB], snr_towards: [20, -8, 44]},
        request_id: @orig_pid
      }
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    packet = rx_packet(%{dest: @us, src: @peer, data: data})
    {:ok, _core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert {:deliver, decoded} =
             Enum.find(effects, fn
               {:deliver, _} -> true
               _ -> false
             end)

    assert decoded.src == @peer
    assert decoded.message.portnum == :TRACEROUTE_APP
    assert decoded.message.request_id == @orig_pid
    assert decoded.message.payload.route == [0xA, 0xB]
    assert decoded.message.payload.snr_towards == [20, -8, 44]
  end

  # ---- handle_rx: TRACEROUTE relay annotation ----

  test "a transiting traceroute request is annotated with our id and snr before relay" do
    packet = rx_packet(%{dest: 0x12345678, data: traceroute_data(%{}, want_response: true)})

    {:ok, core2, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {p, msg} = decode_wire(wire)
    assert p.dest == 0x12345678
    assert p.hop_limit == 2
    assert p.relay_node == :meshtastic.relay_node_byte(@us)
    assert msg.portnum == :TRACEROUTE_APP
    assert msg.payload.route == [@us]
    assert msg.payload.snr_towards == [44]
    assert msg.want_response == true
  end

  test "a transiting traceroute reply is annotated on the route_back arrays" do
    data = traceroute_data(%{route: [0xA], snr_towards: [20]}, request_id: 0x99)
    packet = rx_packet(%{dest: 0x12345678, data: data})

    {:ok, core2, _effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {_p, msg} = decode_wire(wire)
    assert msg.request_id == 0x99
    assert msg.payload.route == [0xA]
    assert msg.payload.snr_towards == [20]
    assert msg.payload.route_back == [@us]
    assert msg.payload.snr_back == [44]
  end

  test "a relayed traceroute request appends to an existing route" do
    data = traceroute_data(%{route: [0xA], snr_towards: [20]}, want_response: true)
    packet = rx_packet(%{dest: 0x12345678, data: data, hop_start: 3, hop_limit: 2})

    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {_p, msg} = decode_wire(wire)
    assert msg.payload.route == [0xA, @us]
    assert msg.payload.snr_towards == [20, 44]
  end

  test "a relayed traceroute pads hops that upstream relays left unannotated" do
    packet =
      rx_packet(%{
        dest: 0x12345678,
        data: traceroute_data(%{}, want_response: true),
        hop_start: 4,
        hop_limit: 2
      })

    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {p, msg} = decode_wire(wire)
    assert p.hop_limit == 1
    assert msg.payload.route == [0xFFFFFFFF, 0xFFFFFFFF, @us]
    assert msg.payload.snr_towards == [-128, -128, 44]
  end

  test "a transiting non-traceroute packet is relayed verbatim (ciphertext unchanged)" do
    packet = rx_packet(%{dest: 0x12345678, data: text_data("hello")})

    {:ok, core2, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {:ok, relayed} = :meshtastic.parse(wire)
    assert relayed.encrypted_data == packet.encrypted_data
    assert relayed.hop_limit == 2
  end

  test "a transiting PKI packet (channel_hash 0) is relayed verbatim, never annotated" do
    packet = rx_packet(%{dest: 0x12345678, channel_hash: 0, data: text_data("dm")})

    {:ok, core2, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {:ok, relayed} = :meshtastic.parse(wire)
    assert relayed.encrypted_data == packet.encrypted_data
  end

  test "a transiting traceroute reply teaches next-hops toward downstream nodes" do
    data = traceroute_data(%{route: [@us, 0xC]}, request_id: 0x99)
    packet = rx_packet(%{dest: 0x12345678, src: 0xD, data: data})

    {:ok, _core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    byte_c = :meshtastic.relay_node_byte(0xC)
    assert {:learn, 0xC, byte_c} in effects
    assert {:learn, 0xD, byte_c} in effects
  end

  # ---- handle_rx: rebroadcast rules ----

  test "a flood is rebroadcast with hop decremented, next_hop cleared, relay stamped" do
    packet = rx_packet(%{dest: @broadcast, next_hop: 0, hop_limit: 3})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert [wire] = drain(core2)
    {:ok, p} = :meshtastic.parse(wire)
    assert p.packet_id == @orig_pid
    assert p.hop_limit == 2
    assert p.next_hop == 0
    assert p.relay_node == :meshtastic.relay_node_byte(@us)
  end

  test "a packet directed at us (next_hop == our byte) is forwarded as a flood, not delivered" do
    our_byte = :meshtastic.relay_node_byte(@us)
    packet = rx_packet(%{dest: 0x11112222, next_hop: our_byte})
    {:ok, core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())

    assert effects == []
    assert [wire] = drain(core2)
    {:ok, p} = :meshtastic.parse(wire)
    assert p.next_hop == 0
    assert p.relay_node == our_byte
    assert p.hop_limit == 2
  end

  test "a packet directed at another node is not rebroadcast" do
    packet = rx_packet(%{dest: 0x11112222, next_hop: 0x77})
    {:ok, core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert effects == []
    assert drain(core2) == []
  end

  test "a flood with hop_limit 0 is not rebroadcast" do
    packet = rx_packet(%{dest: 0x11112222, hop_limit: 0})
    {:ok, core2, []} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert drain(core2) == []
  end

  test "a rebroadcast carries the original ciphertext (no re-encryption)" do
    packet = rx_packet(%{dest: @broadcast})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    [wire] = drain(core2)
    {:ok, p} = :meshtastic.parse(wire)
    assert p.encrypted_data == packet.encrypted_data
  end

  # ---- next-hop routing ----

  defp routing_ack_data(request_id) do
    %{portnum: :ROUTING_APP, payload: %{error_reason: :NONE}, request_id: request_id}
    |> :meshtastic_proto.encode()
    |> :erlang.iolist_to_binary()
  end

  test "rx_route_dests: a recipient (broadcast or to-us) asks for the source's route" do
    bcast = rx_packet(%{dest: @broadcast})
    to_us = rx_packet(%{dest: @us})
    assert :meshtastic_server_core.rx_route_dests(bcast, core()) == [@peer]
    assert :meshtastic_server_core.rx_route_dests(to_us, core()) == [@peer]
  end

  test "rx_route_dests: a packet naming us as next hop asks for the dest's route" do
    our_byte = :meshtastic.relay_node_byte(@us)
    packet = rx_packet(%{dest: 0x11112222, next_hop: our_byte})
    assert :meshtastic_server_core.rx_route_dests(packet, core()) == [0x11112222]
  end

  test "rx_route_dests: a flood or a packet aimed at another node needs no route" do
    assert :meshtastic_server_core.rx_route_dests(
             rx_packet(%{dest: 0x11112222, next_hop: 0}),
             core()
           ) == []

    assert :meshtastic_server_core.rx_route_dests(
             rx_packet(%{dest: 0x11112222, next_hop: 0x77}),
             core()
           ) == []
  end

  test "rx_route_dests: an exhausted-hop packet naming us needs no relay route" do
    our_byte = :meshtastic.relay_node_byte(@us)
    packet = rx_packet(%{dest: 0x11112222, next_hop: our_byte, hop_limit: 0})
    assert :meshtastic_server_core.rx_route_dests(packet, core()) == []
  end

  test "an incoming ACK emits a learn effect for its source via its relay byte" do
    packet = rx_packet(%{dest: @us, relay_node: 0x42, data: routing_ack_data(0x99)})
    {:ok, _core2, effects} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert {:learn, @peer, 0x42} in effects
  end

  test "a PKI ACK to us learns a route from its relay byte (read off the parsed packet)" do
    bob = 0xBBBBBBBB
    {alice_pub, alice_priv} = :crypto.generate_key(:eddh, :x25519)
    {bob_pub, bob_priv} = :crypto.generate_key(:eddh, :x25519)

    wire =
      %{
        dest: bob,
        src: @peer,
        packet_id: 0x12345678,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: 0,
        next_hop: 0,
        relay_node: 0x42,
        data: routing_ack_data(0x99)
      }
      |> :meshtastic.encrypt_pki(alice_priv, bob_pub)
      |> :meshtastic.serialize()

    {:ok, packet} = :meshtastic.parse(wire)
    c = core(node_id: bob, private_key: bob_priv)

    {:ok, _core2, effects} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{peer_key: {:ok, alice_pub}}), c)

    assert {:learn, @peer, 0x42} in effects
  end

  test "a plain text recipient and an ACK without a relay byte emit no learn effect" do
    text = rx_packet(%{dest: @us, data: text_data("hi")})
    {:ok, _c1, e1} = :meshtastic_server_core.handle_rx(text, @attrs, env(), core())
    refute Enum.any?(e1, &match?({:learn, _, _}, &1))

    no_relay = rx_packet(%{dest: @us, relay_node: 0, data: routing_ack_data(0x99)})
    {:ok, _c2, e2} = :meshtastic_server_core.handle_rx(no_relay, @attrs, env(), core())
    refute Enum.any?(e2, &match?({:learn, _, _}, &1))
  end

  test "a duplicate ACK is discarded and does not re-learn" do
    packet = rx_packet(%{dest: @us, relay_node: 0x42, data: routing_ack_data(0x99)})
    {:ok, c1, e1} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), core())
    assert {:learn, @peer, 0x42} in e1

    {:discard, _c2, e2} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{now_ms: 1001}), c1)

    assert e2 == []
  end

  test "handle_send to a node with a known route sets next_hop; unknown floods (0)" do
    {:ok, known, []} =
      :meshtastic_server_core.handle_send(
        @peer,
        text_data("dm"),
        env(%{routes: %{@peer => 0x42}}),
        core()
      )

    {p_known, _} = decode_wire(hd(drain(known)))
    assert p_known.dest == @peer
    assert p_known.next_hop == 0x42

    {:ok, unknown, []} =
      :meshtastic_server_core.handle_send(@peer, text_data("dm"), env(%{routes: %{}}), core())

    {p_unknown, _} = decode_wire(hd(drain(unknown)))
    assert p_unknown.next_hop == 0
  end

  test "handle_send loop-guards a route that points back at our own relay byte" do
    our_byte = :meshtastic.relay_node_byte(@us)

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(
        @peer,
        text_data("dm"),
        env(%{routes: %{@peer => our_byte}}),
        core()
      )

    {p, _} = decode_wire(hd(drain(core2)))
    assert p.next_hop == 0
  end

  test "a learned 0xFF next hop survives (peer id ending in 0x00)" do
    dest = 0x11110000

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(
        dest,
        text_data("dm"),
        env(%{routes: %{dest => 0xFF}}),
        core()
      )

    {p, _} = decode_wire(hd(drain(core2)))
    assert p.next_hop == 0xFF
  end

  test "a packet naming us as next hop is re-directed toward its dest from a known route" do
    our_byte = :meshtastic.relay_node_byte(@us)
    dest = 0x11112222
    packet = rx_packet(%{dest: dest, next_hop: our_byte})

    {:ok, core2, []} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{routes: %{dest => 0x55}}), core())

    {:ok, p} = :meshtastic.parse(hd(drain(core2)))
    assert p.next_hop == 0x55
    assert p.relay_node == our_byte
    assert p.hop_limit == 2
  end

  test "a flood stays a flood on relay even when we know a route to its dest" do
    dest = 0x11112222
    packet = rx_packet(%{dest: dest, next_hop: 0})

    {:ok, core2, []} =
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{routes: %{dest => 0x55}}), core())

    {:ok, p} = :meshtastic.parse(hd(drain(core2)))
    assert p.next_hop == 0
  end

  # ---- handle_send ----

  test "handle_send originates a packet with a deterministic id and advances the counter" do
    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(
        @broadcast,
        text_data("a"),
        env(%{rand22: 0x3FFFFF}),
        core()
      )

    assert [wire] = drain(core2)
    {p, _} = decode_wire(wire)
    assert p.src == @us
    assert p.dest == @broadcast
    # rolling seed was 0, so the first id is counter=1 with our 22 random top bits
    assert (p.packet_id &&& 0x3FF) == 1
    assert p.packet_id >>> 10 == 0x3FFFFF
    assert :meshtastic.decrypt(p, psk()) |> Map.fetch!(:data) == text_data("a")
  end

  test "handle_send with pki encrypts end-to-end and signals PKI on the wire (channel_hash 0)" do
    {our_pub, our_priv} = :crypto.generate_key(:eddh, :x25519)
    {peer_pub, peer_priv} = :crypto.generate_key(:eddh, :x25519)

    c = core(node_id: @us, private_key: our_priv)
    e = env(%{pki: true, peer_key: {:ok, peer_pub}})

    {:ok, core2, []} = :meshtastic_server_core.handle_send(@peer, text_data("secret"), e, c)

    assert [wire] = drain(core2)
    {:ok, p} = :meshtastic.parse(wire)
    assert p.dest == @peer
    assert p.src == @us
    assert p.channel_hash == 0
    assert p.want_ack == false

    {:ok, dec} = :meshtastic.decrypt_pki(p, peer_priv, our_pub)
    assert :meshtastic_proto.decode(dec.data) == %{portnum: :TEXT_MESSAGE_APP, payload: "secret"}
  end

  test "handle_send without pki keeps the channel hash even for a unicast dest" do
    {:ok, core2, []} = :meshtastic_server_core.handle_send(@peer, text_data("dm"), env(), core())

    assert [wire] = drain(core2)
    {:ok, p} = :meshtastic.parse(wire)
    assert p.dest == @peer
    assert p.channel_hash == channel_hash()
    assert :meshtastic.decrypt(p, psk()) |> Map.fetch!(:data) == text_data("dm")
  end

  # ---- handle_periodic ----

  test "handle_periodic broadcasts NodeInfo and re-arms the timer when user_info is set" do
    c = core(node_info: %{user_info: user_info()})
    {core2, effects} = :meshtastic_server_core.handle_periodic(env(), c)

    assert effects == [{:set_timer, 60_000, :periodic}]
    assert [wire] = drain(core2)
    {p, msg} = decode_wire(wire)
    assert p.dest == @broadcast
    assert msg.portnum == :NODEINFO_APP
    refute Map.has_key?(msg, :request_id)
  end

  test "handle_periodic without user_info does nothing and does not re-arm" do
    c = core()
    {core2, effects} = :meshtastic_server_core.handle_periodic(env(), c)
    assert effects == []
    assert drain(core2) == []
  end

  # ---- tx_queue ordering & scheduling seam ----

  test "replies are enqueued before the rebroadcast (FIFO drain order)" do
    # a broadcast NodeInfo want_response is BOTH a recipient (reply) AND floodable
    packet = rx_packet(%{dest: @broadcast, data: nodeinfo_data(want_response: true)})
    c = core(node_info: %{user_info: user_info()})
    {:ok, core2, _} = :meshtastic_server_core.handle_rx(packet, @attrs, env(), c)

    assert [reply_wire, rebroadcast_wire] = drain(core2)

    {reply_pkt, reply_msg} = decode_wire(reply_wire)
    assert reply_pkt.dest == @peer
    assert reply_msg.portnum == :NODEINFO_APP
    assert reply_msg.request_id == @orig_pid

    {:ok, fwd} = :meshtastic.parse(rebroadcast_wire)
    assert fwd.packet_id == @orig_pid
    assert fwd.hop_limit == 2
  end

  test "take_due drains all immediate intents and next_wakeup is then infinity" do
    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())

    {[_intent], core3} = :meshtastic_server_core.take_due(core2, 1_000_000)
    assert :meshtastic_server_core.next_wakeup(core3, 1_000_000) == :infinity
  end

  # ---- take_one_due / has_due (one-at-a-time pump primitives) ----

  test "take_one_due on an empty queue returns :none" do
    assert {:none, _} = :meshtastic_server_core.take_one_due(core(), 1000)
  end

  test "take_one_due pops a single immediate intent and removes it" do
    {:ok, c, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())
    assert {%{payload: _}, c2} = :meshtastic_server_core.take_one_due(c, 1000)
    assert {:none, _} = :meshtastic_server_core.take_one_due(c2, 1000)
  end

  test "take_one_due drains immediates in FIFO order, matching take_due" do
    {:ok, c1, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())
    {:ok, c2, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("b"), env(), c1)

    {[ia, ib], _} = :meshtastic_server_core.take_due(c2, 1000)

    {%{} = i1, c3} = :meshtastic_server_core.take_one_due(c2, 1000)
    {%{} = i2, c4} = :meshtastic_server_core.take_one_due(c3, 1000)
    assert i1.payload == ia.payload
    assert i2.payload == ib.payload
    assert {:none, _} = :meshtastic_server_core.take_one_due(c4, 1000)
  end

  test "take_one_due returns a later due intent, leaving an earlier not-yet-due one queued" do
    # queue head is a deferred rebroadcast (not_before 1208); an immediate own-send follows
    packet = rx_packet(%{data: text_data("flood")})

    {:ok, c1, _} =
      :meshtastic_server_core.handle_rx(
        packet,
        %{rssi: -100, snr: -15},
        env(%{now_ms: 1000, rand22: 0}),
        core()
      )

    {:ok, c2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("now"), env(%{now_ms: 1000}), c1)

    {%{} = first, c3} = :meshtastic_server_core.take_one_due(c2, 1000)
    assert first.not_before == :now
    refute :meshtastic_server_core.has_due(c3, 1000)
    assert :meshtastic_server_core.has_due(c3, 1208)
    {%{} = second, _} = :meshtastic_server_core.take_one_due(c3, 1208)
    assert second.not_before == 1208
  end

  test "has_due reflects whether any intent is due at the given time" do
    refute :meshtastic_server_core.has_due(core(), 1000)

    {:ok, c, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())
    assert :meshtastic_server_core.has_due(c, 1000)

    packet = rx_packet(%{data: text_data("flood")})

    {:ok, c2, _} =
      :meshtastic_server_core.handle_rx(
        packet,
        %{rssi: -100, snr: -15},
        env(%{now_ms: 1000, rand22: 0}),
        core()
      )

    refute :meshtastic_server_core.has_due(c2, 1000)
    assert :meshtastic_server_core.has_due(c2, 1208)
  end

  # ---- handle_tx_results: channel-access retry / back-off ----

  test "a failed send is re-enqueued with a back-off deadline (unbounded retry)" do
    now = 1_000

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())

    {[intent], core3} = :meshtastic_server_core.take_due(core2, now)
    assert Map.get(intent, :attempts, 0) == 0

    core4 =
      :meshtastic_server_core.handle_tx_results(
        [{intent, {:error, :channel_busy}}],
        env(%{now_ms: now}),
        core3
      )

    # not due yet -- it sits behind a future deadline inside the back-off window
    assert {[], _} = :meshtastic_server_core.take_due(core4, now)
    delay = :meshtastic_server_core.next_wakeup(core4, now)
    assert delay >= 0 and delay < 250

    # once the deadline passes it drains again: same bytes, attempts bumped
    {[retried], _} = :meshtastic_server_core.take_due(core4, now + 250)
    assert retried.payload == intent.payload
    assert retried.attempts == 1
  end

  test "repeated channel-busy failures retry forever (never dropped), bumping attempts" do
    now = 1_000

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())

    core_final =
      Enum.reduce(1..10, core2, fn _i, acc ->
        {[intent], acc1} = :meshtastic_server_core.take_due(acc, 10_000_000)

        :meshtastic_server_core.handle_tx_results(
          [{intent, {:error, :channel_busy}}],
          env(%{now_ms: now}),
          acc1
        )
      end)

    # still queued after 10 consecutive failures, attempts kept climbing
    {[intent], _} = :meshtastic_server_core.take_due(core_final, 10_000_000)
    assert intent.attempts == 10
  end

  test "a payload_too_large result is dropped, not retried" do
    now = 1_000

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())

    {[intent], core3} = :meshtastic_server_core.take_due(core2, now)

    core4 =
      :meshtastic_server_core.handle_tx_results(
        [{intent, {:error, :payload_too_large}}],
        env(%{now_ms: now}),
        core3
      )

    assert drain(core4) == []
    assert :meshtastic_server_core.next_wakeup(core4, now) == :infinity
  end

  test "an ok result consumes the intent (nothing re-enqueued)" do
    now = 1_000

    {:ok, core2, []} =
      :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())

    {[intent], core3} = :meshtastic_server_core.take_due(core2, now)
    core4 = :meshtastic_server_core.handle_tx_results([{intent, :ok}], env(%{now_ms: now}), core3)
    assert drain(core4) == []
  end

  test "intents failing together in one batch get decorrelated back-off deadlines" do
    now = 1_000

    {:ok, c1, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("a"), env(), core())
    {:ok, c2, []} = :meshtastic_server_core.handle_send(@broadcast, text_data("b"), env(), c1)
    {[i1, i2], c3} = :meshtastic_server_core.take_due(c2, now)

    core4 =
      :meshtastic_server_core.handle_tx_results(
        [{i1, {:error, :channel_busy}}, {i2, {:error, :channel_busy}}],
        env(%{now_ms: now}),
        c3
      )

    {intents, _} = :meshtastic_server_core.take_due(core4, now + 5_000)
    deadlines = Enum.map(intents, & &1.not_before)
    assert length(deadlines) == 2
    # distinct deadlines -> the shared rand22 was decorrelated per intent
    assert Enum.uniq(deadlines) == deadlines
  end

  # ---- rebroadcast: SNR-weighted contention delay ----

  test "cw_size maps SNR onto the contention window and clamps out-of-range" do
    # snr -20..10 maps linearly onto 3..8, then clamped to [3, 8]
    assert :meshtastic_server_core.cw_size(-20) == 3
    assert :meshtastic_server_core.cw_size(-15) == 3
    assert :meshtastic_server_core.cw_size(0) == 6
    assert :meshtastic_server_core.cw_size(10) == 8
    assert :meshtastic_server_core.cw_size(100) == 8
    assert :meshtastic_server_core.cw_size(-100) == 3
    # no SNR -> treat as a close/strong reception -> longest window
    assert :meshtastic_server_core.cw_size(:undefined) == 8
  end

  test "rebroadcast_delay_ms: 208 ms floor, and a weaker signal has a much shorter ceiling" do
    # delay = 2*CWmax*slot + (rand rem 2^CWsize)*slot; offset = 2*8*13 = 208, slot = 13
    assert :meshtastic_server_core.rebroadcast_delay_ms(-15, 0) == 208
    assert :meshtastic_server_core.rebroadcast_delay_ms(10, 0) == 208
    # far (snr -15, window 8): ceiling 208 + 7*13 = 299
    assert :meshtastic_server_core.rebroadcast_delay_ms(-15, 7) == 299
    # near (snr 10, window 256): ceiling 208 + 255*13 = 3523 -> far relays much sooner
    assert :meshtastic_server_core.rebroadcast_delay_ms(10, 255) == 3523
    # undefined SNR behaves like the strongest (longest) reception
    assert :meshtastic_server_core.rebroadcast_delay_ms(:undefined, 123) ==
             :meshtastic_server_core.rebroadcast_delay_ms(10, 123)
  end

  test "a recipient broadcast is rebroadcast deferred by the SNR delay, tagged with its origin" do
    packet = rx_packet(%{data: text_data("flood")})
    e = env(%{now_ms: 1000, rand22: 0})

    {:ok, core2, _effects} =
      :meshtastic_server_core.handle_rx(packet, %{rssi: -100, snr: -15}, e, core())

    # rand22 = 0 -> delay == the 208 ms floor; deferred, not sent immediately
    assert {[], _} = :meshtastic_server_core.take_due(core2, 1000)
    assert :meshtastic_server_core.next_wakeup(core2, 1000) == 208
    {[intent], _} = :meshtastic_server_core.take_due(core2, 1208)
    assert intent.not_before == 1208
    assert intent.ref == {@peer, @orig_pid}
  end

  test "a not-for-us forwarded packet is also deferred and tagged with its origin" do
    packet = rx_packet(%{dest: 0x12345678})
    e = env(%{now_ms: 5000, rand22: 0})

    {:ok, core2, []} =
      :meshtastic_server_core.handle_rx(packet, %{rssi: -90, snr: 0}, e, core())

    {[intent], _} = :meshtastic_server_core.take_due(core2, 5208)
    assert intent.not_before == 5208
    assert intent.ref == {@peer, @orig_pid}
  end

  # ---- pure primitives ----

  test "next_packet_id wraps the 10-bit counter and carries the random top bits" do
    assert {pid, 0} = :meshtastic_server_core.next_packet_id(0x3FF, 0)
    assert (pid &&& 0x3FF) == 0
    assert {pid2, 5} = :meshtastic_server_core.next_packet_id(4, 0xABCDE)
    assert (pid2 &&& 0x3FF) == 5
    assert pid2 >>> 10 == 0xABCDE
  end

  test "update_last_seen detects duplicates and bumps newer timestamps" do
    assert {false, m1} = :meshtastic_server_core.update_last_seen(%{}, @peer, 1, 100)
    assert m1 == %{@peer => %{1 => 100}}
    assert {true, m2} = :meshtastic_server_core.update_last_seen(m1, @peer, 1, 150)
    assert m2 == %{@peer => %{1 => 150}}
    assert {true, ^m2} = :meshtastic_server_core.update_last_seen(m2, @peer, 1, 150)
    assert {false, m3} = :meshtastic_server_core.update_last_seen(m2, @peer, 2, 200)
    assert m3 == %{@peer => %{1 => 150, 2 => 200}}
  end

  test "prune_expired_last_seen drops entries older than 30s and removes empty sources" do
    m = %{@peer => %{1 => 100}, @us => %{2 => 50}}
    # at now=130: peer/1 age=30 kept, us/2 age=80 dropped (source removed)
    assert :meshtastic_server_core.prune_expired_last_seen(m, 130) == %{@peer => %{1 => 100}}
    # at now=131: peer/1 age=31 dropped too
    assert :meshtastic_server_core.prune_expired_last_seen(m, 131) == %{}
  end
end
