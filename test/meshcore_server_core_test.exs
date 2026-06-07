defmodule MeshcoreServerCoreTest do
  # Pure unit tests for the MeshCore functional core. No process, no mock radio:
  # each test passes an explicit core state + Env and asserts on the returned
  # {Reply, CoreState, Effects}. The gen_server wiring is covered by
  # meshcore_server_test.
  use ExUnit.Case, async: true

  # Captured on-air frames: f1 (control discover_req), f8 (group text
  # "testd: hello"), f4 (advert from node "testd").
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
  @attrs %{rssi: -66, snr: 6}

  defp channel_key, do: :meshcore_protocol.default_public_channel_key()

  defp core(opts \\ []) do
    {core, _effects} = :meshcore_server_core.init(opts)
    core
  end

  test "init returns a bare core and arms nothing" do
    assert {_core, []} = :meshcore_server_core.init([])
  end

  test "rx delivers decrypted group text with rssi/snr merged in" do
    {:ok, pkt} = :meshcore_protocol.parse(@grp_txt)
    {:ok, _core, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())

    assert [{:deliver, delivered}] = effects
    assert delivered.text == "testd: hello"
    assert delivered.timestamp == 1_672_533_147
    assert delivered.rssi == -66
    assert delivered.snr == 6
    refute Map.has_key?(delivered, :ciphertext)
    refute Map.has_key?(delivered, :cipher_mac)
  end

  test "rx delivers an advert with sig_ok and rssi/snr merged in" do
    {:ok, pkt} = :meshcore_protocol.parse(@advert)
    {:ok, _core, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())

    assert [{:deliver, delivered}] = effects
    assert delivered.sig_ok == true
    assert delivered.name == "testd"
    assert delivered.rssi == -66
    assert delivered.snr == 6
  end

  test "rx flags an undecryptable group text and keeps the ciphertext" do
    {:ok, pkt} = :meshcore_protocol.parse(@grp_txt)

    {:ok, _core, effects} =
      :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core(channel_key: <<0::128>>))

    assert [{:deliver, delivered}] = effects
    assert delivered.decrypt_error == :bad_mac
    assert Map.has_key?(delivered, :ciphertext)
    refute Map.has_key?(delivered, :text)
  end

  test "enrich passes non-decodable frame types through unchanged" do
    {:ok, pkt} = :meshcore_protocol.parse(@control)
    assert :meshcore_server_core.enrich(pkt, channel_key()) == pkt
  end
end
