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

  # ---- handle_send_group_text ----

  test "send group text enqueues a flood grp_txt with our name prepended" do
    opts = identity_opts()

    {reply, core1, effects} =
      :meshcore_server_core.handle_send_group_text(
        "hi there",
        %{wall_s: 1_700_000_000},
        core(opts)
      )

    assert reply == :ok
    assert effects == []

    assert [wire] = drain(core1)
    {:ok, pkt} = :meshcore_protocol.parse(wire)
    assert pkt.route == :flood
    assert pkt.type == :grp_txt
    assert pkt.channel_hash == 0x11

    {:ok, dec} = :meshcore_protocol.decrypt(pkt)
    assert dec.text == "pocketOS T1: hi there"
    assert dec.timestamp == 1_700_000_000

    # our own TX is remembered, so a repeater echo of it is dropped
    assert {:ok, _core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)
  end

  test "send group text without a name is rejected" do
    {reply, _core, effects} =
      :meshcore_server_core.handle_send_group_text("hi", %{wall_s: 1}, core())

    assert reply == {:error, :no_identity}
    assert effects == []
  end

  # ---- handle_send_dm (untracked) ----

  test "send dm enqueues a flood txt_msg the recipient can decrypt" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {recipient_pub, recipient_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {reply, core1, effects} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "ping",
        tx_env(%{wall_s: 1_700_000_000}),
        core(opts)
      )

    assert reply == :ok
    assert effects == []

    assert [wire] = drain(core1)
    {:ok, pkt} = :meshcore_protocol.parse(wire)
    assert pkt.type == :txt_msg
    assert pkt.route == :flood
    <<dest_first, _::binary>> = recipient_pub
    <<src_first, _::binary>> = our_pub
    assert pkt.dest_hash == dest_first
    assert pkt.src_hash == src_first

    {:ok, secret} = :meshcore_protocol.shared_secret(recipient_priv, our_pub)
    {:ok, dec} = :meshcore_protocol.decrypt_shared(secret, pkt)
    assert dec.text == "ping"
    assert dec.timestamp == 1_700_000_000
  end

  test "send dm without an identity is rejected" do
    {recipient_pub, _} = :crypto.generate_key(:eddsa, :ed25519)

    {reply, _core, []} =
      :meshcore_server_core.handle_send_dm(recipient_pub, "x", tx_env(%{wall_s: 1}), core())

    assert reply == {:error, :no_identity}
  end

  test "send dm caches the recipient secret, enabling inbound decrypt without an advert" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {recipient_pub, recipient_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, core1, _} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "hi",
        tx_env(%{wall_s: 1_700_000_000}),
        core(opts)
      )

    # inbound DM from the recipient (whose advert we never heard) decrypts via
    # the contact the send cached
    inbound = dm_frame(our_pub, recipient_pub, recipient_priv, "reply text", 1_700_000_100)
    {:ok, pkt} = :meshcore_protocol.parse(inbound)
    {:ok, _core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:deliver, delivered}] = effects
    assert delivered.text == "reply text"
    assert delivered.sender_pubkey == recipient_pub
  end

  # ---- handle_send_dm: delivery tracking ----

  test "a tracked send enqueues with a ref and arms an ack timeout" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {recipient_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    notify = {self(), make_ref()}

    {reply, core1, effects} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "track me",
        tx_env(%{wall_s: 1_700_000_000, notify: notify}),
        core(opts)
      )

    assert reply == :ok
    assert [{:set_timer, interval, {:ack_timeout, ack_hash}}] = effects
    assert is_integer(interval) and interval > 0
    assert byte_size(ack_hash) == 4

    # the queued intent is tagged with the ack hash, so the ack can cancel it
    {[intent], _} = :meshcore_server_core.take_due(core1, 1_000_000)
    assert intent.ref == ack_hash

    # the ack hash is exactly what the recipient computes over our plaintext + key
    <<expected::4-bytes, _::binary>> =
      :meshcore_protocol.ack_payload(
        %{timestamp: 1_700_000_000, txt_type: 0, attempt: 0, text: "track me"},
        our_pub,
        0
      )

    assert ack_hash == expected
  end

  # ---- handle_ack_timeout ----

  test "ack timeout retransmits the same payload up to the limit, then naks" do
    opts = identity_opts()
    {recipient_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    notify = {self(), make_ref()}

    {:ok, core1, [{:set_timer, _, {:ack_timeout, ack_hash}}]} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "hi",
        tx_env(%{wall_s: 1, notify: notify}),
        core(opts)
      )

    {[first], core2} = :meshcore_server_core.take_due(core1, 1_000_000)

    # two retransmits (DM_RETX = 3 total transmissions), each the same bytes + ref
    core_after =
      Enum.reduce(1..2, core2, fn _, c ->
        {c1, effects} = :meshcore_server_core.handle_ack_timeout(ack_hash, c)
        assert [{:set_timer, _, {:ack_timeout, ^ack_hash}}] = effects
        {[retx], c2} = :meshcore_server_core.take_due(c1, 1_000_000)
        assert retx.payload == first.payload
        assert retx.ref == ack_hash
        c2
      end)

    # the next timeout gives up
    {_core, effects} = :meshcore_server_core.handle_ack_timeout(ack_hash, core_after)
    assert effects == [{:notify, notify, {:nak, :timeout}}]
  end

  test "ack timeout for an unknown hash is a no-op" do
    assert {_core, []} = :meshcore_server_core.handle_ack_timeout(<<0, 0, 0, 0>>, core())
  end

  # ---- handle_rx: tracked-dm ack resolution ----

  test "rx resolves a tracked dm when the bundled path-return ack arrives" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {recipient_pub, recipient_priv} = :crypto.generate_key(:eddsa, :ed25519)
    notify = {self(), make_ref()}

    {:ok, core1, [{:set_timer, _, {:ack_timeout, ack_hash}}]} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "deliver me",
        tx_env(%{wall_s: 1_700_000_000, notify: notify}),
        core(opts)
      )

    # the recipient floods a path-return bundling the 4-byte ack hash
    wire =
      path_return_frame(our_pub, recipient_pub, recipient_priv, <<>>, ack_hash <> <<0, 0xAB>>)

    {:ok, pkt} = :meshcore_protocol.parse(wire)
    {:ok, core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:notify, ^notify, {:ack, %{}}}, {:deliver, _}] = effects
    # the queued first transmission is cancelled
    assert {[], _} = :meshcore_server_core.take_due(core2, 1_000_000)
  end

  test "rx resolves a tracked dm when a discrete ack arrives" do
    opts = identity_opts()
    {recipient_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    notify = {self(), make_ref()}

    {:ok, core1, [{:set_timer, _, {:ack_timeout, ack_hash}}]} =
      :meshcore_server_core.handle_send_dm(
        recipient_pub,
        "x",
        tx_env(%{wall_s: 1, notify: notify}),
        core(opts)
      )

    ack = %{
      route: :flood,
      type: :ack,
      version: 0,
      hash_size: 1,
      path: <<>>,
      ack: ack_hash <> <<0, 1>>
    }

    {:ok, pkt} = :meshcore_protocol.parse(:meshcore_protocol.serialize(ack))
    {:ok, core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:notify, ^notify, {:ack, %{}}}, {:deliver, _}] = effects
    assert {[], _} = :meshcore_server_core.take_due(core2, 1_000_000)
  end

  test "rx ignores an ack that matches no pending send" do
    opts = identity_opts()

    ack = %{
      route: :flood,
      type: :ack,
      version: 0,
      hash_size: 1,
      path: <<>>,
      ack: <<9, 9, 9, 9, 0, 1>>
    }

    {:ok, pkt} = :meshcore_protocol.parse(:meshcore_protocol.serialize(ack))
    {:ok, _core, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))

    refute Enum.any?(effects, &match?({:notify, _, _}, &1))
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

  test "rx leaves a wrong-channel group text unconsumed" do
    {:ok, pkt} = :meshcore_protocol.parse(@grp_txt)
    core = core(channel_key: <<0::128>>)

    assert {:next, core1, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core)
    # not remembered as seen: a retransmission must fall through too
    assert {:next, _core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core1)
  end

  test "rx leaves an aliased meshtastic unicast unconsumed (on-air capture)" do
    # meshtastic packet to node 0x2B310D17: first byte 0x17 parses as a
    # version-0 grp_txt/transport_direct meshcore header
    wire =
      <<23, 13, 49, 43, 132, 70, 49, 67, 100, 114, 97, 70, 66, 31, 0, 132, 3, 146, 181, 236, 212,
        146, 20, 175, 224, 235, 99, 101, 34, 24, 53, 70, 219, 163, 15, 182, 220, 232, 252, 26,
        229, 0, 16, 59, 91, 157, 148, 141, 31, 71, 136, 237, 246, 222, 153, 151, 143, 238, 205,
        80, 31, 204, 79, 25, 41, 0, 16, 13, 88, 24, 47, 244, 160, 56, 165, 23, 238, 81, 220, 66,
        70, 34, 140, 88, 141, 137, 19, 53, 164, 173, 107, 168, 25, 133, 162, 166, 9, 9, 134>>

    {:ok, pkt} = :meshcore_protocol.parse(wire)
    assert {:next, _core, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())
  end

  test "rx leaves an advert with a broken signature unconsumed" do
    <<head::binary-40, sig_byte, rest::binary>> = @advert

    {:ok, pkt} =
      :meshcore_protocol.parse(<<head::binary, Bitwise.bxor(sig_byte, 1), rest::binary>>)

    assert {:next, _core, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())
  end

  test "rx consumes a 6-byte ack but leaves other ack sizes unconsumed" do
    {:ok, ack} = :meshcore_protocol.parse(<<0x0D, 0x00, 1, 2, 3, 4, 5, 6>>)

    assert {:ok, _core, [{:deliver, _}]} =
             :meshcore_server_core.handle_rx(ack, @attrs, %{}, core())

    {:ok, short} = :meshcore_protocol.parse(<<0x0D, 0x00, 1, 2, 3>>)
    assert {:next, _core, []} = :meshcore_server_core.handle_rx(short, @attrs, %{}, core())
  end

  test "rx leaves unhandled frame types (raw_custom) unconsumed" do
    {:ok, pkt} = :meshcore_protocol.parse(<<0x3D, 0x00, 1, 2, 3>>)
    assert {:next, _core, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())
  end

  test "enrich passes non-decodable frame types through unchanged" do
    {:ok, pkt} = :meshcore_protocol.parse(@control)
    assert :meshcore_server_core.enrich(pkt, channel_key()) == pkt
  end

  # ---- handle_rx: dedup ----

  test "rx drops a frame already seen" do
    {:ok, pkt} = :meshcore_protocol.parse(@grp_txt)
    {:ok, core1, [{:deliver, _}]} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())
    assert {:ok, _core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core1)
  end

  test "rx drops the same payload heard again via another route" do
    <<0x15, 0x00, payload::binary>> = @grp_txt
    {:ok, flood} = :meshcore_protocol.parse(@grp_txt)
    {:ok, direct} = :meshcore_protocol.parse(<<0x16, 0x01, 0xAB, payload::binary>>)

    {:ok, core1, [{:deliver, _}]} = :meshcore_server_core.handle_rx(flood, @attrs, %{}, core())
    assert {:ok, _core2, []} = :meshcore_server_core.handle_rx(direct, @attrs, %{}, core1)
  end

  test "rx drops our own advert echoed back by a repeater" do
    {core1, _} =
      :meshcore_server_core.handle_periodic(%{wall_s: 1_700_000_000}, core(identity_opts()))

    [wire] = drain(core1)
    <<header, 0x00, payload::binary>> = wire
    {:ok, echo} = :meshcore_protocol.parse(<<header, 0x01, 0xAB, payload::binary>>)

    assert {:ok, _core2, []} = :meshcore_server_core.handle_rx(echo, @attrs, %{}, core1)
  end

  test "delivered frames carry the dedup packet hash" do
    {:ok, pkt} = :meshcore_protocol.parse(@grp_txt)

    {:ok, _core, [{:deliver, delivered}]} =
      :meshcore_server_core.handle_rx(pkt, @attrs, %{}, core())

    assert byte_size(delivered.packet_hash) == 8
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

    # five scans (each with a fresh tag) inside one window -> four replies,
    # the fifth denied
    c5 =
      Enum.reduce(1..5, core(opts), fn i, c ->
        {:ok, c2, _} = :meshcore_server_core.handle_rx(scan(i), @attrs, tx_env(), c)
        c2
      end)

    assert length(drain(c5)) == 4

    # a scan after the window lapses opens a fresh window and replies again
    {:ok, c6, _} =
      :meshcore_server_core.handle_rx(scan(6), @attrs, tx_env(%{mono_ms: 130_000}), c5)

    assert length(drain(c6)) == 5
  end

  defp scan(tag) do
    {:ok, req} = :meshcore_protocol.parse(<<0x2E, 0x00, 0x80, 0xFF, tag::32>>)
    req
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

  # ---- handle_rx: anon_req ----

  test "rx decrypts an anon_req addressed to us and never replies" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)
    frame = anon_req_frame(our_pub, sender_pub, sender_priv, 1_700_000_000, "hello test direct")
    {:ok, pkt} = :meshcore_protocol.parse(frame)

    {:ok, core1, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))

    assert [{:deliver, delivered}] = effects
    assert delivered.type == :anon_req
    assert delivered.timestamp == 1_700_000_000
    assert delivered.req_data == "hello test direct"
    assert delivered.sender_pubkey == sender_pub
    assert delivered.rssi == -66
    refute Map.has_key?(delivered, :ciphertext)
    refute Map.has_key?(delivered, :cipher_mac)
    assert drain(core1) == []
  end

  test "rx leaves an anon_req not addressed to us unconsumed" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)
    <<0x1E, 0x00, dest, rest::binary>> = anon_req_frame(our_pub, sender_pub, sender_priv, 1, "x")
    {:ok, pkt} = :meshcore_protocol.parse(<<0x1E, 0x00, rem(dest + 1, 256), rest::binary>>)

    assert {:next, core1, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))
    assert drain(core1) == []
  end

  test "rx leaves an undecryptable anon_req addressed to us unconsumed" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    {_, wrong_priv} = :crypto.generate_key(:eddsa, :ed25519)
    frame = anon_req_frame(our_pub, sender_pub, wrong_priv, 1, "x")
    {:ok, pkt} = :meshcore_protocol.parse(frame)

    assert {:next, core1, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))
    assert drain(core1) == []
  end

  # ---- handle_rx: direct messages ----

  test "rx decrypts a direct message from a contact learned via advert" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, adv} = :meshcore_protocol.parse(advert_frame(sender_pub, sender_priv, "testd", 100))
    {:ok, core1, _} = :meshcore_server_core.handle_rx(adv, @attrs, tx_env(), core(opts))

    dm = dm_frame(our_pub, sender_pub, sender_priv, "hello test direct", 1_700_000_000)
    {:ok, pkt} = :meshcore_protocol.parse(dm)
    {:ok, core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:deliver, delivered}] = effects
    assert delivered.type == :txt_msg
    assert delivered.text == "hello test direct"
    assert delivered.timestamp == 1_700_000_000
    assert delivered.txt_type == 0
    assert delivered.sender_pubkey == sender_pub
    assert delivered.sender_name == "testd"
    refute Map.has_key?(delivered, :ciphertext)
    refute Map.has_key?(delivered, :decrypt_error)

    assert [_ack] = drain(core2)
  end

  test "rx leaves a direct message from an unknown sender unconsumed" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, pkt} = :meshcore_protocol.parse(dm_frame(our_pub, sender_pub, sender_priv, "x", 1))

    assert {:next, core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))
    assert drain(core2) == []
  end

  test "rx leaves a direct message that decrypts under no known contact unconsumed" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)
    {_imp_pub, imp_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, adv} = :meshcore_protocol.parse(advert_frame(sender_pub, sender_priv, "testd", 100))
    {:ok, core1, _} = :meshcore_server_core.handle_rx(adv, @attrs, tx_env(), core(opts))

    {:ok, pkt} = :meshcore_protocol.parse(dm_frame(our_pub, sender_pub, imp_priv, "x", 1))

    assert {:next, core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)
    assert drain(core2) == []
  end

  test "rx leaves a direct message not addressed to us unconsumed" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)

    <<h, pl, dest, rest::binary>> = dm_frame(our_pub, sender_pub, sender_priv, "x", 1)
    {:ok, pkt} = :meshcore_protocol.parse(<<h, pl, rem(dest + 1, 256), rest::binary>>)

    assert {:next, core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core(opts))
    assert drain(core2) == []
  end

  test "rx resolves src_hash collisions by trying every matching contact" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {a_pub, a_priv} = :crypto.generate_key(:eddsa, :ed25519)
    <<first, _::binary>> = a_pub
    {b_pub, b_priv} = keypair_with_first(first)

    {:ok, adv_a} = :meshcore_protocol.parse(advert_frame(a_pub, a_priv, "a", 100))
    {:ok, adv_b} = :meshcore_protocol.parse(advert_frame(b_pub, b_priv, "b", 100))
    {:ok, c1, _} = :meshcore_server_core.handle_rx(adv_a, @attrs, tx_env(), core(opts))
    {:ok, c2, _} = :meshcore_server_core.handle_rx(adv_b, @attrs, tx_env(), c1)

    {:ok, pkt} = :meshcore_protocol.parse(dm_frame(our_pub, a_pub, a_priv, "from a", 1))

    {:ok, _c3, [{:deliver, delivered}]} =
      :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), c2)

    assert delivered.text == "from a"
    assert delivered.sender_name == "a"
    assert delivered.sender_pubkey == a_pub
  end

  test "rx ignores a replayed advert with an older timestamp" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, newer} =
      :meshcore_protocol.parse(advert_frame(sender_pub, sender_priv, "new-name", 200))

    {:ok, older} =
      :meshcore_protocol.parse(advert_frame(sender_pub, sender_priv, "old-name", 100))

    {:ok, c1, _} = :meshcore_server_core.handle_rx(newer, @attrs, tx_env(), core(opts))
    {:ok, c2, [{:deliver, _}]} = :meshcore_server_core.handle_rx(older, @attrs, tx_env(), c1)

    {:ok, pkt} = :meshcore_protocol.parse(dm_frame(our_pub, sender_pub, sender_priv, "x", 1))

    {:ok, _c3, [{:deliver, delivered}]} =
      :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), c2)

    assert delivered.sender_name == "new-name"
  end

  test "the contact table caps out by evicting the least recently heard" do
    opts = identity_opts()
    our_pub = Keyword.fetch!(opts, :public_key)
    {first_pub, first_priv} = :crypto.generate_key(:eddsa, :ed25519)

    {:ok, adv} = :meshcore_protocol.parse(advert_frame(first_pub, first_priv, "first", 100))
    {:ok, c0, _} = :meshcore_server_core.handle_rx(adv, @attrs, tx_env(%{mono_ms: 0}), core(opts))

    c64 =
      Enum.reduce(1..64, c0, fn i, c ->
        {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
        {:ok, a} = :meshcore_protocol.parse(advert_frame(pub, priv, "n#{i}", 100 + i))
        {:ok, c2, _} = :meshcore_server_core.handle_rx(a, @attrs, tx_env(%{mono_ms: i}), c)
        c2
      end)

    {:ok, pkt} = :meshcore_protocol.parse(dm_frame(our_pub, first_pub, first_priv, "x", 1))

    # the evicted contact can no longer decrypt for us, so the DM is not ours
    assert {:next, _c, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), c64)
  end

  # ---- handle_rx: path-return decrypt ----

  test "rx decrypts a path-return addressed to us and surfaces the bundled ack" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()
    ack = <<1, 2, 3, 4, 5, 6>>

    wire = path_return_frame(our_pub, sender_pub, sender_priv, <<>>, ack)
    {:ok, pkt} = :meshcore_protocol.parse(wire)
    {:ok, _core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:deliver, delivered}] = effects
    assert delivered.type == :path
    assert delivered.extra_type == :ack
    assert binary_part(delivered.extra, 0, 6) == ack
    assert delivered.return_path == <<>>
    assert delivered.sender_pubkey == sender_pub
    refute Map.has_key?(delivered, :ciphertext)
  end

  test "rx leaves a path-return addressed to another node unconsumed" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()
    <<our_first, _::binary>> = our_pub
    {other_pub, _} = keypair_with_first(rem(our_first + 1, 256))

    wire = path_return_frame(other_pub, sender_pub, sender_priv, <<>>, <<1, 2, 3, 4, 5, 6>>)
    {:ok, pkt} = :meshcore_protocol.parse(wire)
    assert {:next, _core2, []} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)
  end

  # ---- direct-message ack ----

  test "rx acks a flood direct message with a path return" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "hello test direct", 1_700_000_000)
    {:ok, pkt} = :meshcore_protocol.parse(dm)

    {:ok, core2, _} =
      :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(%{rand22: 0x1AB}), core1)

    # scheduled 200 ms after rx (mono_ms 5_000)
    {[], core3} = :meshcore_server_core.take_due(core2, 5_199)
    {[intent], _} = :meshcore_server_core.take_due(core3, 5_200)

    {:ok, reply} = :meshcore_protocol.parse(intent.payload)
    assert reply.type == :path
    assert reply.route == :flood
    assert reply.path == <<>>
    <<sender_first, _::binary>> = sender_pub
    <<our_first, _::binary>> = our_pub
    assert reply.dest_hash == sender_first
    assert reply.src_hash == our_first

    {:ok, secret} = :meshcore_protocol.shared_secret(sender_priv, our_pub)
    <<mac::2-bytes, _::binary>> = :crypto.mac(:hmac, :sha256, secret, reply.ciphertext)
    assert reply.cipher_mac == mac

    <<aes_key::16-bytes, _::binary>> = secret
    plain = :crypto.crypto_one_time(:aes_128_ecb, aes_key, reply.ciphertext, false)
    expect4 = expect_ack4(1_700_000_000, "hello test direct", sender_pub)
    assert plain == <<0x00, 3>> <> expect4 <> <<0, 0xAB>> <> <<0::size(8 * 8)>>
  end

  test "rx echoes a multi-hop route back in the path return" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "x", 1, %{path: <<0xAA, 0xBB>>})
    {:ok, pkt} = :meshcore_protocol.parse(dm)
    {:ok, core2, _} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [wire] = drain(core2)
    {:ok, reply} = :meshcore_protocol.parse(wire)
    {:ok, secret} = :meshcore_protocol.shared_secret(sender_priv, our_pub)
    <<aes_key::16-bytes, _::binary>> = secret
    plain = :crypto.crypto_one_time(:aes_128_ecb, aes_key, reply.ciphertext, false)
    assert <<0x02, 0xAA, 0xBB, 3, _::binary>> = plain
  end

  test "rx acks a direct-routed direct message with a discrete flooded ack" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "another direct", 99, %{route: :direct})
    {:ok, pkt} = :meshcore_protocol.parse(dm)

    {:ok, core2, _} =
      :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(%{rand22: 0x1AB}), core1)

    {[], core3} = :meshcore_server_core.take_due(core2, 5_199)
    {[intent], _} = :meshcore_server_core.take_due(core3, 5_200)

    {:ok, reply} = :meshcore_protocol.parse(intent.payload)
    assert reply.type == :ack
    assert reply.route == :flood
    assert reply.path == <<>>
    assert reply.ack == expect_ack4(99, "another direct", sender_pub) <> <<0, 0xAB>>
  end

  test "the ack carries the attempt byte hidden after the text's NUL" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "hi" <> <<0, 5>>, 1)
    {:ok, pkt} = :meshcore_protocol.parse(dm)
    {:ok, core2, _} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [wire] = drain(core2)
    {:ok, reply} = :meshcore_protocol.parse(wire)
    {:ok, secret} = :meshcore_protocol.shared_secret(sender_priv, our_pub)
    <<aes_key::16-bytes, _::binary>> = secret
    plain = :crypto.crypto_one_time(:aes_128_ecb, aes_key, reply.ciphertext, false)
    expect4 = expect_ack4(1, "hi", sender_pub)
    assert <<0x00, 3, ^expect4::4-bytes, 5, 0, _::binary>> = plain
  end

  test "rx drops its own ack echoed back by a repeater" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "x", 1)
    {:ok, pkt} = :meshcore_protocol.parse(dm)
    {:ok, core2, _} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [wire] = drain(core2)
    {:ok, echo} = :meshcore_protocol.parse(wire)
    assert {:ok, _core3, []} = :meshcore_server_core.handle_rx(echo, @attrs, tx_env(), core2)
  end

  test "rx does not ack a non-plain direct message" do
    {core1, our_pub, sender_pub, sender_priv} = core_with_contact()

    dm = dm_frame(our_pub, sender_pub, sender_priv, "cmd", 1, %{txt_type: 1})
    {:ok, pkt} = :meshcore_protocol.parse(dm)
    {:ok, core2, effects} = :meshcore_server_core.handle_rx(pkt, @attrs, tx_env(), core1)

    assert [{:deliver, delivered}] = effects
    assert delivered.txt_type == 1
    assert drain(core2) == []
  end

  defp core_with_contact do
    opts = identity_opts()
    {sender_pub, sender_priv} = :crypto.generate_key(:eddsa, :ed25519)
    {:ok, adv} = :meshcore_protocol.parse(advert_frame(sender_pub, sender_priv, "testd", 100))
    {:ok, core1, _} = :meshcore_server_core.handle_rx(adv, @attrs, tx_env(), core(opts))
    {core1, Keyword.fetch!(opts, :public_key), sender_pub, sender_priv}
  end

  # Recomputed from scratch: hash of timestamp | flags | text | sender key.
  defp expect_ack4(timestamp, text, sender_pub) do
    binary_part(:crypto.hash(:sha256, <<timestamp::32-little, 0>> <> text <> sender_pub), 0, 4)
  end

  defp advert_frame(pub, priv, name, timestamp) do
    appdata = :meshcore_protocol.encode_advert_appdata(%{node_type: :chat, name: name})

    :meshcore_protocol.serialize(
      :meshcore_protocol.sign_advert(
        %{
          route: :flood,
          type: :advert,
          version: 0,
          hash_size: 1,
          path: <<>>,
          public_key: pub,
          timestamp: timestamp,
          appdata: appdata
        },
        priv
      )
    )
  end

  defp dm_frame(recipient_pub, src_pub, seal_priv, text, timestamp, extra \\ %{}) do
    {:ok, secret} = :meshcore_protocol.shared_secret(seal_priv, recipient_pub)
    <<dest_hash, _::binary>> = recipient_pub
    <<src_hash, _::binary>> = src_pub

    :meshcore_protocol.serialize(
      :meshcore_protocol.encrypt_shared(
        secret,
        Map.merge(
          %{
            route: :flood,
            type: :txt_msg,
            version: 0,
            hash_size: 1,
            path: <<>>,
            dest_hash: dest_hash,
            src_hash: src_hash,
            timestamp: timestamp,
            text: text
          },
          extra
        )
      )
    )
  end

  defp path_return_frame(recipient_pub, src_pub, seal_priv, return_path, ack) do
    {:ok, secret} = :meshcore_protocol.shared_secret(seal_priv, recipient_pub)
    <<dest_hash, _::binary>> = recipient_pub
    <<src_hash, _::binary>> = src_pub

    :meshcore_protocol.serialize(
      :meshcore_protocol.encrypt_shared(secret, %{
        route: :flood,
        type: :path,
        version: 0,
        hash_size: 1,
        path: <<>>,
        dest_hash: dest_hash,
        src_hash: src_hash,
        return_path: return_path,
        extra_type: :ack,
        extra: ack
      })
    )
  end

  defp keypair_with_first(byte) do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)

    case pub do
      <<^byte, _::binary>> -> {pub, priv}
      _ -> keypair_with_first(byte)
    end
  end

  defp anon_req_frame(recipient_pub, sender_pub, sender_priv, timestamp, req_data) do
    {:ok, secret} = :meshcore_protocol.shared_secret(sender_priv, recipient_pub)
    inner = <<timestamp::32-little>> <> req_data
    pad = rem(16 - rem(byte_size(inner), 16), 16)
    <<aes_key::16-bytes, _::binary>> = secret
    ct = :crypto.crypto_one_time(:aes_128_ecb, aes_key, inner <> :binary.copy(<<0>>, pad), true)
    <<mac::2-bytes, _::binary>> = :crypto.mac(:hmac, :sha256, secret, ct)
    <<dest_hash, _::binary>> = recipient_pub
    <<0x1E, 0x00, dest_hash, sender_pub::binary, mac::binary, ct::binary>>
  end
end
