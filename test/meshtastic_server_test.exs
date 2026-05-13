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
    def broadcast(:mock, payload) do
      :io.format("Going to send ~p~n", [payload])
      :ok
    end

    def broadcast(pid, payload) do
      :io.format("Going to send ~p~n", [payload])
      send(pid, payload)
      :ok
    end
  end

  test "handle a text message" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock}, callbacks: TestCallbacks)

    iface = {:mock, :undefined}

    payload_0 =
      <<255, 255, 255, 255, 132, 70, 49, 67, 214, 65, 11, 101, 99, 31, 0, 0, 128, 234, 186, 192,
        51, 252, 136, 181, 75, 187, 247, 119, 124, 9, 148, 58, 83, 188, 81, 148>>

    :ok = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -28, snr: 11})

    assert_receive(
      %{
        want_ack: false,
        dest: 4_294_967_295,
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
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock}, callbacks: TestCallbacks)

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
end
