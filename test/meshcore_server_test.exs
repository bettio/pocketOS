defmodule MeshcoreServerTest do
  use ExUnit.Case, async: true

  # Captured on-air frames: f1 (control discover_req), f8 (group text "testd: hello"),
  # f4 (advert from node "testd"). Frame enrichment is covered in the core tests
  # (meshcore_server_core_test); here we test the gen_server wiring + presentation.
  @control <<0x2E, 0x00, 0x80, 0xFF, 0xBC, 0xC7, 0x32, 0x10>>
  @grp_txt <<0x15, 0x00, 0x11, 0x58, 0x9A, 0x1E, 0x2D, 0x2F, 0x5C, 0xE4, 0x9A, 0xC1, 0x68, 0xF9,
             0xB3, 0x7E, 0x7F, 0xBD, 0x1A, 0xBD, 0xE2, 0xA1, 0x0F, 0x37, 0xA2, 0x24, 0xF5, 0x06,
             0x4D, 0x77, 0x23, 0x54, 0x63, 0x3C, 0x4C, 0x1F, 0x22>>
  @advert <<0x12, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C,
            0xCA, 0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5,
            0x73, 0x5F, 0x9D, 0xBB, 0xE1, 0xF4, 0x36, 0xD4, 0xB0, 0x63, 0xD8, 0xE4, 0xE1, 0xFD,
            0xB8, 0xB5, 0x92, 0x77, 0x8C, 0x86, 0x9D, 0xD0, 0xD4, 0xEE, 0x1A, 0x2E, 0x3F, 0x20,
            0x2A, 0xFB, 0x88, 0x38, 0xAC, 0xE6, 0x70, 0x5D, 0xCC, 0xDE, 0xFE, 0x87, 0xE2, 0xA2,
            0x5E, 0xE1, 0x66, 0x95, 0x85, 0x63, 0x36, 0x5F, 0x1A, 0x1B, 0x82, 0x71, 0x22, 0xAC,
            0xFD, 0x99, 0x96, 0x35, 0x12, 0x4A, 0x2E, 0x2C, 0x5E, 0xD9, 0x8D, 0x8E, 0x85, 0xF3,
            0xC0, 0xF2, 0xA8, 0x01, 0x81, 0x74, 0x65, 0x73, 0x74, 0x64>>

  defp channel_key, do: :meshcore_protocol.default_public_channel_key()

  defp identity_opts do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    [public_key: pub, private_key: priv, name: "pocketOS T1"]
  end

  defmodule CbSink do
    def message_cb(pkt), do: send(:meshcore_cb_sink, {:message_cb, pkt})
  end

  # Radio stub that forwards each broadcast to the handle pid (the test process).
  defmodule TestRadio do
    def broadcast(pid, payload) do
      send(pid, {:tx, payload})
      :ok
    end
  end

  test "delivered frames are handed to the callbacks module" do
    Process.register(self(), :meshcore_cb_sink)
    {:ok, srv} = :meshcore_server.start_link({:test_iface, :test_mod, self()}, callbacks: CbSink)

    :ok =
      :meshcore_server.handle_payload(srv, {:test_mod, self()}, @grp_txt, %{rssi: -66, snr: 6})

    assert_receive {:message_cb, %{type: :grp_txt, text: "testd: hello", rssi: -66}}
  end

  test "handle_payload consumes (:ok) frames that parse and passes through (:next) garbage" do
    {:ok, srv} = :meshcore_server.start_link({:test_iface, :test_mod, self()}, [])
    iface = {:test_mod, self()}
    attrs = %{rssi: -66, snr: 6}

    assert :meshcore_server.handle_payload(srv, iface, @control, attrs) == :ok
    assert :meshcore_server.handle_payload(srv, iface, @grp_txt, attrs) == :ok
    assert :meshcore_server.handle_payload(srv, iface, @advert, attrs) == :ok
    assert :meshcore_server.handle_payload(srv, iface, <<>>, attrs) == :next
  end

  test "send_group_text broadcasts a decryptable flood grp_txt" do
    {:ok, srv} = :meshcore_server.start_link({TestRadio, TestRadio, self()}, identity_opts())

    assert :meshcore_server.send_group_text(srv, "hello mesh") == :ok

    assert_receive {:tx, wire}
    {:ok, pkt} = :meshcore_protocol.parse(wire)
    assert pkt.type == :grp_txt
    assert pkt.route == :flood
    {:ok, dec} = :meshcore_protocol.decrypt(pkt)
    assert dec.text == "pocketOS T1: hello mesh"
  end

  test "for_log drops the bulky advert binaries but keeps the decoded fields" do
    {:ok, pkt} = :meshcore_protocol.parse(@advert)
    logged = :meshcore_server.for_log(:meshcore_server_core.enrich(pkt, channel_key()))

    refute Map.has_key?(logged, :public_key)
    refute Map.has_key?(logged, :signature)
    refute Map.has_key?(logged, :appdata)
    assert logged.name == "testd"
    assert logged.node_type == :chat
    assert logged.sig_ok == true
  end

  test "for_log drops the anon_req sender pubkey and ciphertext" do
    logged =
      :meshcore_server.for_log(%{
        type: :anon_req,
        sender_pubkey: <<0::256>>,
        ciphertext: <<1, 2, 3>>,
        req_data: "hi"
      })

    refute Map.has_key?(logged, :sender_pubkey)
    refute Map.has_key?(logged, :ciphertext)
    assert logged.req_data == "hi"
  end
end
