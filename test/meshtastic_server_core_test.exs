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

  defp env(extra \\ %{}), do: Map.merge(%{now: 1000, rand22: 0x2ABCDE}, extra)

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
      :meshtastic_server_core.handle_rx(packet, @attrs, env(%{now: 1001}), core1b)

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
        env(%{now: now}),
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
          env(%{now: now}),
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
        env(%{now: now}),
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
    core4 = :meshtastic_server_core.handle_tx_results([{intent, :ok}], env(%{now: now}), core3)
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
        env(%{now: now}),
        c3
      )

    {intents, _} = :meshtastic_server_core.take_due(core4, now + 5_000)
    deadlines = Enum.map(intents, & &1.not_before)
    assert length(deadlines) == 2
    # distinct deadlines -> the shared rand22 was decorrelated per intent
    assert Enum.uniq(deadlines) == deadlines
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
