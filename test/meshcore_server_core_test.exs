defmodule MeshcoreServerCoreTest do
  # Pure unit tests for the MeshCore functional core. No process, no mock radio,
  # no timers: each test passes an explicit core state + Env and asserts on the
  # returned {Reply, CoreState, Effects} and the resulting tx_queue (read back
  # via take_due/2). The gen_server wiring is covered by meshcore_server_test.
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

  # ---- helpers ----

  defp channel_key, do: :meshcore_protocol.default_public_channel_key()

  defp identity_opts do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    [public_key: pub, private_key: priv, name: "pocketOS T1"]
  end

  defp core(opts \\ []) do
    {core, _effects} = :meshcore_server_core.init(opts)
    core
  end

  defp drain(core) do
    {intents, _core2} = :meshcore_server_core.take_due(core, 1_000_000)
    Enum.map(intents, & &1.payload)
  end

  defp tx_env(extra \\ %{}), do: Map.merge(%{mono_ms: 5_000, rand22: 0}, extra)

  # ---- init ----

  test "init arms the first periodic timer when a signing identity is present" do
    {_core, effects} = :meshcore_server_core.init(identity_opts())
    assert effects == [{:set_timer, 500, :periodic}]
  end

  test "init without an identity arms nothing" do
    assert {_core, []} = :meshcore_server_core.init([])
  end

  test "init with keys but no name does not advertise" do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    assert {_core, []} = :meshcore_server_core.init(public_key: pub, private_key: priv)
  end

  # ---- handle_periodic: signed flood advert ----

  test "periodic builds a signed flood advert and re-arms the 60s timer" do
    opts = identity_opts()
    ts = 1_700_000_500

    {core2, effects} = :meshcore_server_core.handle_periodic(%{wall_s: ts}, core(opts))
    assert effects == [{:set_timer, 60_000, :periodic}]

    assert [wire] = drain(core2)
    {:ok, adv} = :meshcore_protocol.parse(wire)
    assert adv.route == :flood
    assert adv.type == :advert
    assert adv.version == 0
    assert adv.path == <<>>
    assert adv.public_key == Keyword.fetch!(opts, :public_key)
    assert adv.timestamp == ts
    assert adv.node_type == :chat
    assert adv.name == "pocketOS T1"
    assert :meshcore_protocol.verify_advert(adv)
  end

  test "periodic without an identity emits no advert and no timer" do
    c = core()
    assert {^c, []} = :meshcore_server_core.handle_periodic(%{wall_s: 1}, c)
    assert drain(c) == []
  end

  # ---- handle_tx_results ----

  defp taken_advert_intent do
    {core1, _} = :meshcore_server_core.handle_periodic(%{wall_s: 1}, core(identity_opts()))
    {intent, core2} = :meshcore_server_core.take_one_due(core1, 1_000_000)
    {intent, core2}
  end

  test "a successful tx leaves the queue empty" do
    {intent, core2} = taken_advert_intent()
    core3 = :meshcore_server_core.handle_tx_results([{intent, :ok}], tx_env(), core2)
    assert drain(core3) == []
  end

  test "payload_too_large is dropped permanently" do
    {intent, core2} = taken_advert_intent()

    core3 =
      :meshcore_server_core.handle_tx_results(
        [{intent, {:error, :payload_too_large}}],
        tx_env(),
        core2
      )

    assert drain(core3) == []
  end

  test "a transient tx error re-enqueues with a back-off deadline" do
    {intent, core2} = taken_advert_intent()
    assert drain(core2) == []

    core3 =
      :meshcore_server_core.handle_tx_results([{intent, {:error, :timeout}}], tx_env(), core2)

    {due_before, _} = :meshcore_server_core.take_due(core3, 4_999)
    assert due_before == []

    {[reenqueued], _} = :meshcore_server_core.take_due(core3, 5_000)
    assert reenqueued.payload == intent.payload
    assert reenqueued.attempts == 1
  end

  # ---- handle_rx: enrich + attributes ----

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

  # ---- handle_rx: discover reply ----

  test "rx answers a matching network scan with a zero-hop DISCOVER_RESP" do
    opts = identity_opts()
    {:ok, req} = :meshcore_protocol.parse(@control)
    {:ok, core1, effects} = :meshcore_server_core.handle_rx(req, @attrs, tx_env(), core(opts))

    # the request itself is still delivered for logging
    assert [{:deliver, _}] = effects

    # and a DISCOVER_RESP is enqueued, echoing the tag with our identity and SNR
    assert [wire] = drain(core1)
    {:ok, resp} = :meshcore_protocol.parse(wire)
    assert resp.route == :direct
    assert resp.path == <<>>
    assert resp.type == :control
    assert resp.sub_type == :discover_resp
    assert resp.node_type == :chat
    assert resp.tag == <<0xBC, 0xC7, 0x32, 0x10>>
    assert resp.reported_snr == 24
    assert resp.public_key == Keyword.fetch!(opts, :public_key)
  end

  test "rx replies only when the type filter selects our (chat) node type" do
    opts = identity_opts()

    # 0x04 = 1 <<< ADV_TYPE_REPEATER -- chat bit (0x02) not set
    repeater_scan = <<0x2E, 0x00, 0x80, 0x04, 0xBC, 0xC7, 0x32, 0x10>>
    {:ok, req1} = :meshcore_protocol.parse(repeater_scan)
    {:ok, core1, _} = :meshcore_server_core.handle_rx(req1, @attrs, tx_env(), core(opts))
    assert drain(core1) == []

    # 0x02 = 1 <<< ADV_TYPE_CHAT -- selects us
    chat_scan = <<0x2E, 0x00, 0x80, 0x02, 0xBC, 0xC7, 0x32, 0x10>>
    {:ok, req2} = :meshcore_protocol.parse(chat_scan)
    {:ok, core2, _} = :meshcore_server_core.handle_rx(req2, @attrs, tx_env(), core(opts))
    assert [_wire] = drain(core2)
  end

  test "rx rate-limits discover replies to a fixed window" do
    opts = identity_opts()
    {:ok, req} = :meshcore_protocol.parse(@control)

    # five scans inside one window -> four replies, the fifth denied
    c5 =
      Enum.reduce(1..5, core(opts), fn _i, c ->
        {:ok, c2, _} = :meshcore_server_core.handle_rx(req, @attrs, tx_env(), c)
        c2
      end)

    assert length(drain(c5)) == 4

    # a scan after the window lapses opens a fresh window and replies again
    {:ok, c6, _} = :meshcore_server_core.handle_rx(req, @attrs, tx_env(%{mono_ms: 130_000}), c5)
    assert length(drain(c6)) == 5
  end

  test "rx does not answer a scan without a signing identity" do
    {:ok, req} = :meshcore_protocol.parse(@control)
    {:ok, core1, _} = :meshcore_server_core.handle_rx(req, @attrs, tx_env(), core())
    assert drain(core1) == []
  end

  # ---- handle_rx: airtime-proportional reply delay ----

  defp reply_not_before(extra_opts, env_extra) do
    opts = identity_opts() ++ extra_opts
    env = tx_env(env_extra)
    {:ok, req} = :meshcore_protocol.parse(@control)
    {:ok, core1, _} = :meshcore_server_core.handle_rx(req, @attrs, env, core(opts))
    {[intent], _} = :meshcore_server_core.take_due(core1, 100_000_000)
    intent.not_before - env.mono_ms
  end

  test "reply delay is zero on slot 0 and scales by airtime slot" do
    # slot = rand22 rem 5; slots 0 and 5 -> no delay
    assert reply_not_before([], %{rand22: 0}) == 0
    assert reply_not_before([], %{rand22: 5}) == 0

    # slots 1 and 2 -> non-zero, in a 1:2 ratio (discrete airtime slots)
    d1 = reply_not_before([], %{rand22: 1})
    d2 = reply_not_before([], %{rand22: 2})
    assert d1 > 0
    assert d2 == 2 * d1
  end

  test "reply delay grows with the spreading factor (airtime)" do
    d_sf9 = reply_not_before([spreading_factor: 9], %{rand22: 1})
    d_sf12 = reply_not_before([spreading_factor: 12], %{rand22: 1})
    assert d_sf12 > d_sf9
  end
end
