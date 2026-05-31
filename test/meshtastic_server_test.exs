defmodule MeshtasticServerTest do
  use ExUnit.Case
  import Bitwise
  doctest PocketOS

  defmodule TestCallbacks do
    def message_cb(msg) do
      send(:meshtastic_server_tester, msg)
    end

    def peer_public_key(node_id) do
      case :persistent_term.get({:test_peer_pub, node_id}, nil) do
        nil -> {:error, :not_found}
        pub -> {:ok, pub}
      end
    end
  end

  defmodule TestRadio do
    # Pop the next programmed broadcast result for this radio handle, defaulting
    # to :ok (the original always-succeed behaviour). Tests program a queue of
    # results via :persistent_term to exercise the retry/back-off path.
    defp next_result(handle) do
      case :persistent_term.get({:test_radio_results, handle}, []) do
        [r | rest] ->
          :persistent_term.put({:test_radio_results, handle}, rest)
          r

        [] ->
          :ok
      end
    end

    def broadcast(:mock, payload) do
      :io.format("Going to send ~p~n", [payload])
      next_result(:mock)
    end

    def broadcast(pid, payload) do
      # Only a successful send is observable to the test (mirrors a real radio:
      # a busy/failed TX never reaches the air), so the retry path is provable.
      case next_result(pid) do
        :ok ->
          :io.format("Going to send ~p~n", [payload])
          send(pid, payload)
          :ok

        {:error, _} = err ->
          :io.format("Simulated tx failure ~p~n", [err])
          err
      end
    end
  end

  test "handle a text message" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock},
        callbacks: TestCallbacks,
        node_id: 0xDEADCAFE
      )

    iface = {:mock, :undefined}

    payload_0 =
      <<255, 255, 255, 255, 132, 70, 49, 67, 214, 65, 11, 101, 99, 31, 0, 0, 128, 234, 186, 192,
        51, 252, 136, 181, 75, 187, 247, 119, 124, 9, 148, 58, 83, 188, 81, 148>>

    :ok = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -28, snr: 11})

    assert_receive(
      %{
        want_ack: false,
        dest: 4_294_967_295,
        rssi: -28,
        snr: 11,
        message: %{portnum: :TEXT_MESSAGE_APP, payload: <<"ciaone a tutti">>}
      },
      5000
    )

    payload_1 = <<0>>
    :next = :meshtastic_server.handle_payload(server, iface, payload_1, %{rssi: -20, snr: 15})

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "duplicates are discarded" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock},
        callbacks: TestCallbacks,
        node_id: 0xDEADCAFE
      )

    iface = {:mock, :undefined}

    payload_0 =
      <<255, 255, 255, 255, 132, 70, 49, 67, 214, 65, 11, 101, 99, 31, 0, 0, 128, 234, 186, 192,
        51, 252, 136, 181, 75, 187, 247, 119, 124, 9, 148, 58, 83, 188, 81, 148>>

    :ok = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -28, snr: 11})
    :discard = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -10, snr: 12})

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "decrypt a PKI direct message" do
    Process.register(self(), :meshtastic_server_tester)

    alice_node_id = 0xAAAAAAAA
    bob_node_id = 0xBBBBBBBB
    {alice_pub, alice_priv} = :crypto.generate_key(:eddh, :x25519)
    {bob_pub, bob_priv} = :crypto.generate_key(:eddh, :x25519)
    :persistent_term.put({:test_peer_pub, alice_node_id}, alice_pub)

    text_data =
      %{portnum: :TEXT_MESSAGE_APP, payload: "hi from alice"}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    packet = %{
      dest: bob_node_id,
      src: alice_node_id,
      packet_id: 0x12345678,
      hop_start: 3,
      via_mqtt: false,
      want_ack: false,
      hop_limit: 3,
      channel_hash: 0,
      next_hop: 0,
      relay_node: 0,
      data: text_data
    }

    wire_payload =
      packet
      |> :meshtastic.encrypt_pki(alice_priv, bob_pub)
      |> :meshtastic.serialize()

    {:ok, server} =
      :meshtastic_server.start_link(
        {TestRadio, TestRadio, :mock},
        callbacks: TestCallbacks,
        node_id: bob_node_id,
        private_key: bob_priv
      )

    iface = {:mock, :undefined}

    :ok = :meshtastic_server.handle_payload(server, iface, wire_payload, %{rssi: -28, snr: 11})

    assert_receive(
      %{
        dest: ^bob_node_id,
        src: ^alice_node_id,
        message: %{portnum: :TEXT_MESSAGE_APP, payload: "hi from alice"}
      },
      5000
    )

    # Tamper: flipping any byte must cause the CCM tag check to fail.
    <<head::binary-16, first_cipher_byte, rest::binary>> = wire_payload
    tampered = <<head::binary, first_cipher_byte ^^^ 0x01, rest::binary>>

    :discard =
      :meshtastic_server.handle_payload(server, iface, tampered, %{rssi: -28, snr: 11})

    refute_receive(_, 200)

    :ok = :gen_server.stop(server)
    :persistent_term.erase({:test_peer_pub, alice_node_id})
    Process.unregister(:meshtastic_server_tester)
  end

  test "messages are correctly sent" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()}, callbacks: TestCallbacks)

    iface = {:mock, :undefined}

    test_data =
      <<8, 1, 18, 14, 99, 105, 97, 111, 110, 101, 32, 97, 32, 116, 117, 116, 116, 105, 72, 0>>

    :ok = :meshtastic_server.send(server, 0xFFFFFFFF, test_data)

    receive do
      bin when is_binary(bin) ->
        assert :meshtastic.parse(bin) |> elem(1) |> :meshtastic.decrypt() |> Map.fetch!(:data) ==
                 test_data
    after
      5000 -> assert_receive(:fail, 0)
    end

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "a busy channel is retried until it succeeds (message not lost, server survives)" do
    Process.register(self(), :meshtastic_server_tester)

    # First broadcast reports the channel busy; the back-off retry must succeed.
    :persistent_term.put({:test_radio_results, self()}, [{:error, :channel_busy}])

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()}, callbacks: TestCallbacks)

    test_data =
      <<8, 1, 18, 14, 99, 105, 97, 111, 110, 101, 32, 97, 32, 116, 117, 116, 116, 105, 72, 0>>

    :ok = :meshtastic_server.send(server, 0xFFFFFFFF, test_data)

    # The first attempt failed (nothing observable); only the retry delivers it.
    receive do
      bin when is_binary(bin) ->
        assert :meshtastic.parse(bin) |> elem(1) |> :meshtastic.decrypt() |> Map.fetch!(:data) ==
                 test_data
    after
      5000 -> flunk("payload was never re-broadcast after channel_busy")
    end

    assert Process.alive?(server)

    :ok = :gen_server.stop(server)
    :persistent_term.erase({:test_radio_results, self()})
    Process.unregister(:meshtastic_server_tester)
  end

  test "a NodeInfo request with want_response gets an automatic reply" do
    Process.register(self(), :meshtastic_server_tester)

    us = 0xDEADCAFE
    peer = 0xAAAAAAAA
    orig_pid = 0x11223344

    our_user_info = %{
      id: "!deadcafe",
      long_name: "pocketOS test",
      short_name: "dc",
      hw_model: 50,
      role: :CLIENT,
      is_licensed: false,
      macaddr: <<0xDE, 0xAD, 0xCA, 0xFE, 0x00, 0x01>>,
      public_key: :binary.copy(<<0>>, 32)
    }

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()},
        callbacks: TestCallbacks,
        node_id: us,
        node_info: %{user_info: our_user_info}
      )

    %{hash: channel_hash} = :meshtastic.default_long_fast_channel()

    req_data =
      %{
        portnum: :NODEINFO_APP,
        payload: %{id: "!aaaaaaaa", long_name: "peer", short_name: "pe", hw_model: 50},
        want_response: true
      }
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    wire =
      %{
        dest: us,
        src: peer,
        packet_id: orig_pid,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: channel_hash,
        next_hop: 0,
        relay_node: 0,
        data: req_data
      }
      |> :meshtastic.encrypt(:meshtastic.default_long_fast_psk())
      |> :meshtastic.serialize()

    iface = {:mock, :undefined}
    :ok = :meshtastic_server.handle_payload(server, iface, wire, %{rssi: -28, snr: 11})

    # want_response is exposed to the callback
    assert_receive(
      %{
        src: ^peer,
        want_response: true,
        message: %{portnum: :NODEINFO_APP, payload: %{long_name: "peer"}}
      },
      5000
    )

    # and we unicast our own NodeInfo back to the requester, tagged with its packet id
    reply_wire =
      receive do
        bin when is_binary(bin) -> bin
      after
        5000 -> flunk("expected a NodeInfo reply")
      end

    {:ok, reply_pkt} = :meshtastic.parse(reply_wire)
    assert reply_pkt.dest == peer
    assert reply_pkt.src == us

    reply_msg =
      reply_pkt
      |> :meshtastic.decrypt(:meshtastic.default_long_fast_psk())
      |> Map.fetch!(:data)
      |> :meshtastic_proto.decode()

    assert reply_msg.portnum == :NODEINFO_APP
    assert reply_msg.request_id == orig_pid
    assert reply_msg.payload.long_name == "pocketOS test"
    assert reply_msg.payload.short_name == "dc"

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "a non-NodeInfo packet with want_response gets no reply" do
    Process.register(self(), :meshtastic_server_tester)

    us = 0xDEADCAFE
    peer = 0xAAAAAAAA

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()},
        callbacks: TestCallbacks,
        node_id: us,
        node_info: %{user_info: %{id: "!deadcafe", long_name: "pocketOS test", short_name: "dc"}}
      )

    %{hash: channel_hash} = :meshtastic.default_long_fast_channel()

    req_data =
      %{portnum: :TEXT_MESSAGE_APP, payload: "ping", want_response: true}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    wire =
      %{
        dest: us,
        src: peer,
        packet_id: 0x55667788,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: channel_hash,
        next_hop: 0,
        relay_node: 0,
        data: req_data
      }
      |> :meshtastic.encrypt(:meshtastic.default_long_fast_psk())
      |> :meshtastic.serialize()

    iface = {:mock, :undefined}
    :ok = :meshtastic_server.handle_payload(server, iface, wire, %{rssi: -28, snr: 11})

    # exposure still works for non-NodeInfo portnums
    assert_receive(
      %{src: ^peer, want_response: true, message: %{portnum: :TEXT_MESSAGE_APP, payload: "ping"}},
      5000
    )

    # but the auto-reply is NodeInfo-only, so nothing is transmitted back
    # (the periodic NodeInfo broadcast fires at 500ms, well after this 200ms window)
    receive do
      bin when is_binary(bin) -> flunk("did not expect a reply, got: #{inspect(bin)}")
    after
      200 -> :ok
    end

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "rebroadcast floods a broadcast and stamps our relay_node" do
    Process.register(self(), :meshtastic_server_tester)

    us = 0xDEADCAFE
    peer = 0xAAAAAAAA

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()},
        callbacks: TestCallbacks,
        node_id: us
      )

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: "relay me"}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    %{hash: channel_hash} = :meshtastic.default_long_fast_channel()

    # A broadcast we should forward (flood). relay_node gets overwritten with
    # ours; next_hop stays 0.
    wire =
      %{
        dest: 0xFFFFFFFF,
        src: peer,
        packet_id: 0x0BADF00D,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: channel_hash,
        next_hop: 0,
        relay_node: 0xAA,
        data: data
      }
      |> :meshtastic.encrypt(:meshtastic.default_long_fast_psk())
      |> :meshtastic.serialize()

    iface = {:mock, :undefined}
    :ok = :meshtastic_server.handle_payload(server, iface, wire, %{rssi: -20, snr: 5})

    rebroadcast =
      receive do
        bin when is_binary(bin) -> bin
      after
        5000 -> flunk("expected a rebroadcast")
      end

    {:ok, p} = :meshtastic.parse(rebroadcast)
    assert p.packet_id == 0x0BADF00D
    # hop_limit decremented, next_hop stays 0 (flood), relay_node = our last byte
    assert p.hop_limit == 2
    assert p.next_hop == 0
    assert p.relay_node == 0xFE

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "rebroadcast clears next_hop when the packet was directed at us" do
    Process.register(self(), :meshtastic_server_tester)

    us = 0xDEADCAFE
    peer = 0xAAAAAAAA
    dest = 0x11112222
    our_byte = :meshtastic.relay_node_byte(us)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()},
        callbacks: TestCallbacks,
        node_id: us
      )

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: "fwd"}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    %{hash: channel_hash} = :meshtastic.default_long_fast_channel()

    # A DM for someone else that names us as the next hop.
    wire =
      %{
        dest: dest,
        src: peer,
        packet_id: 0x0BADF00D,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: channel_hash,
        next_hop: our_byte,
        relay_node: 0xAA,
        data: data
      }
      |> :meshtastic.encrypt(:meshtastic.default_long_fast_psk())
      |> :meshtastic.serialize()

    iface = {:mock, :undefined}
    :ok = :meshtastic_server.handle_payload(server, iface, wire, %{rssi: -20, snr: 5})

    rebroadcast =
      receive do
        bin when is_binary(bin) -> bin
      after
        5000 -> flunk("expected a rebroadcast")
      end

    {:ok, p} = :meshtastic.parse(rebroadcast)
    # next_hop cleared so the flood continues; relay_node still stamped to us
    assert p.next_hop == 0
    assert p.relay_node == our_byte
    assert p.hop_limit == 2

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "no rebroadcast when the packet is directed at another node" do
    Process.register(self(), :meshtastic_server_tester)

    us = 0xDEADCAFE
    peer = 0xAAAAAAAA
    dest = 0x11112222

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, self()},
        callbacks: TestCallbacks,
        node_id: us
      )

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: "not mine"}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    %{hash: channel_hash} = :meshtastic.default_long_fast_channel()

    # A directed DM whose next_hop names a different node (0x77, not our 0xFE).
    wire =
      %{
        dest: dest,
        src: peer,
        packet_id: 0x0BADF00D,
        hop_start: 3,
        via_mqtt: false,
        want_ack: false,
        hop_limit: 3,
        channel_hash: channel_hash,
        next_hop: 0x77,
        relay_node: 0xAA,
        data: data
      }
      |> :meshtastic.encrypt(:meshtastic.default_long_fast_psk())
      |> :meshtastic.serialize()

    iface = {:mock, :undefined}
    :ok = :meshtastic_server.handle_payload(server, iface, wire, %{rssi: -20, snr: 5})

    # The named relay handles it, not us -- nothing should be transmitted.
    receive do
      bin when is_binary(bin) -> flunk("did not expect a rebroadcast, got: #{inspect(bin)}")
    after
      200 -> :ok
    end

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end
end
