defmodule MeshcoreProtocolTest do
  use ExUnit.Case, async: true

  # Captured on-air MeshCore frames (f1–f13), byte-for-byte as received.
  @f1 <<0x2E, 0x00, 0x80, 0xFF, 0xBC, 0xC7, 0x32, 0x10>>
  @f2 <<0x2E, 0x00, 0x80, 0xFF, 0xBD, 0x0F, 0x58, 0x18>>
  @f3 <<0x2E, 0x00, 0x80, 0xFF, 0x94, 0xB6, 0xE6, 0x05>>
  @f4 <<0x12, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
        0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
        0x9D, 0xBB, 0xE1, 0xF4, 0x36, 0xD4, 0xB0, 0x63, 0xD8, 0xE4, 0xE1, 0xFD, 0xB8, 0xB5, 0x92,
        0x77, 0x8C, 0x86, 0x9D, 0xD0, 0xD4, 0xEE, 0x1A, 0x2E, 0x3F, 0x20, 0x2A, 0xFB, 0x88, 0x38,
        0xAC, 0xE6, 0x70, 0x5D, 0xCC, 0xDE, 0xFE, 0x87, 0xE2, 0xA2, 0x5E, 0xE1, 0x66, 0x95, 0x85,
        0x63, 0x36, 0x5F, 0x1A, 0x1B, 0x82, 0x71, 0x22, 0xAC, 0xFD, 0x99, 0x96, 0x35, 0x12, 0x4A,
        0x2E, 0x2C, 0x5E, 0xD9, 0x8D, 0x8E, 0x85, 0xF3, 0xC0, 0xF2, 0xA8, 0x01, 0x81, 0x74, 0x65,
        0x73, 0x74, 0x64>>
  @f5 <<0x12, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
        0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
        0x9D, 0xBB, 0xE1, 0xF4, 0x4B, 0xD4, 0xB0, 0x63, 0xD1, 0x10, 0xC1, 0x11, 0x65, 0xA9, 0xAF,
        0x67, 0x4C, 0x04, 0xAB, 0x3D, 0x05, 0x98, 0x1B, 0xE4, 0x0E, 0xF1, 0xF8, 0x52, 0xD0, 0x45,
        0x8A, 0x18, 0x38, 0x41, 0x28, 0x5B, 0xE9, 0x03, 0x13, 0x77, 0x4A, 0x29, 0x81, 0x90, 0x41,
        0x47, 0xDB, 0x57, 0x99, 0x48, 0x54, 0xE9, 0x09, 0x17, 0x46, 0xC2, 0x53, 0xFA, 0xDE, 0xFF,
        0xC3, 0xA1, 0x84, 0x8F, 0x0F, 0x21, 0x9C, 0xAE, 0xA5, 0x20, 0x31, 0x0B, 0x81, 0x74, 0x65,
        0x73, 0x74, 0x64>>
  @f6 <<0x11, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
        0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
        0x9D, 0xBB, 0xE1, 0xF4, 0x5D, 0xD4, 0xB0, 0x63, 0xBF, 0x84, 0xCF, 0x39, 0x6A, 0xEF, 0x65,
        0xA3, 0x6A, 0xAB, 0x58, 0x88, 0x8E, 0x3F, 0xA1, 0x60, 0xAC, 0xC6, 0x10, 0x4B, 0x86, 0xBA,
        0x26, 0x6F, 0xFE, 0x9D, 0xA1, 0x54, 0x6B, 0x8A, 0xDD, 0x92, 0xF7, 0x6E, 0x8A, 0x8E, 0x90,
        0x29, 0xE8, 0x47, 0x49, 0xD2, 0xA5, 0x65, 0x3D, 0xE5, 0x6C, 0xC1, 0x56, 0x0A, 0xE5, 0x61,
        0x53, 0x49, 0x38, 0xCC, 0xD6, 0xAF, 0xF6, 0xE5, 0xD2, 0xCF, 0xE9, 0x06, 0x81, 0x74, 0x65,
        0x73, 0x74, 0x64>>
  @f7 <<0x2E, 0x00, 0x80, 0xFF, 0x59, 0x77, 0xC8, 0x7F>>
  @f8 <<0x15, 0x00, 0x11, 0x58, 0x9A, 0x1E, 0x2D, 0x2F, 0x5C, 0xE4, 0x9A, 0xC1, 0x68, 0xF9, 0xB3,
        0x7E, 0x7F, 0xBD, 0x1A, 0xBD, 0xE2, 0xA1, 0x0F, 0x37, 0xA2, 0x24, 0xF5, 0x06, 0x4D, 0x77,
        0x23, 0x54, 0x63, 0x3C, 0x4C, 0x1F, 0x22>>
  @f9 <<0x12, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
        0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
        0x9D, 0xBB, 0xE1, 0xF4, 0xD6, 0xD4, 0xB0, 0x63, 0x15, 0x8D, 0xFE, 0x67, 0xD0, 0x30, 0x95,
        0x95, 0x6E, 0x1B, 0xC4, 0x2D, 0x3E, 0xB4, 0x22, 0xEF, 0xF6, 0x78, 0x0C, 0xD8, 0xED, 0xDD,
        0xF7, 0x3A, 0xE1, 0xFE, 0xD6, 0x60, 0xFF, 0x6A, 0x3A, 0x8C, 0x7A, 0x66, 0x00, 0x40, 0xF1,
        0x0A, 0x79, 0x1B, 0x37, 0xDE, 0xE6, 0x25, 0xE2, 0x5D, 0x1E, 0x59, 0xD2, 0x3A, 0xD5, 0x78,
        0x33, 0x93, 0x11, 0x79, 0x59, 0xA3, 0x7A, 0x5C, 0x40, 0xE7, 0xCB, 0x0F, 0x81, 0x74, 0x65,
        0x73, 0x74, 0x74, 0x65, 0x73, 0x74>>
  @f10 <<0x15, 0x00, 0x11, 0x98, 0x1F, 0xA0, 0x6A, 0xC9, 0x6A, 0xC6, 0x8B, 0xFC, 0xFD, 0x3E, 0x35,
         0x9F, 0x4D, 0xB2, 0x6C, 0xE7, 0xA8, 0x26, 0x24, 0x3E, 0x33, 0xE3, 0x33, 0xD1, 0xE4, 0xB4,
         0xC6, 0x17, 0xF9, 0x1F, 0x6B, 0x93, 0xCC>>
  @f11 <<0x11, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
         0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
         0x9D, 0xBB, 0xE1, 0xF4, 0x64, 0xD5, 0xB0, 0x63, 0x15, 0xD0, 0xD1, 0x28, 0xB8, 0x15, 0xB8,
         0x98, 0x48, 0xAA, 0x7B, 0xB7, 0xCA, 0x6B, 0x30, 0x08, 0x03, 0xED, 0x8D, 0x3E, 0xE1, 0x25,
         0x28, 0x53, 0x33, 0xC5, 0x26, 0x3F, 0x17, 0x75, 0x5F, 0x6F, 0xCE, 0x3C, 0xAD, 0x9E, 0x6B,
         0x3D, 0xD5, 0x09, 0xD1, 0x6F, 0xE7, 0x03, 0x1B, 0xE8, 0x64, 0x67, 0x31, 0xE7, 0xFD, 0x17,
         0xA4, 0x25, 0xBE, 0x9A, 0xCE, 0xBF, 0x60, 0xEC, 0x96, 0xE4, 0x0C, 0x0D, 0x81, 0x74, 0x65,
         0x73, 0x74, 0x74, 0x65, 0x73, 0x74>>
  @f12 <<0x12, 0x00, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
         0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73, 0x5F,
         0x9D, 0xBB, 0xE1, 0xF4, 0xB4, 0xD5, 0xB0, 0x63, 0x47, 0x0B, 0x21, 0xCF, 0x9C, 0x62, 0x6E,
         0x45, 0x82, 0x10, 0xD7, 0xBC, 0xFB, 0xB4, 0x98, 0xA3, 0x9D, 0x1B, 0x56, 0xCF, 0x46, 0xFF,
         0x20, 0x06, 0xC6, 0xCF, 0x9D, 0x9E, 0x5D, 0xD3, 0xFD, 0x96, 0x57, 0x51, 0x46, 0x45, 0xCB,
         0xBB, 0xB0, 0x6F, 0x1E, 0x6F, 0x6C, 0x63, 0x46, 0x99, 0x6F, 0xB4, 0xDD, 0xE4, 0x69, 0x4D,
         0x79, 0x34, 0xB2, 0xF9, 0x57, 0x3A, 0xBD, 0x8B, 0x91, 0x3A, 0xF2, 0x01, 0x81, 0x74, 0x65,
         0x73, 0x74, 0x74, 0x65, 0x73, 0x74>>
  @f13 <<0x15, 0x00, 0x11, 0x74, 0x58, 0x61, 0x66, 0xCB, 0x89, 0x95, 0xFE, 0x6E, 0x49, 0x15, 0x91,
         0x73, 0x6A, 0xB4, 0x21, 0x55, 0x6A, 0xE1, 0x71, 0x7C, 0x3F, 0xBF, 0x5D, 0xA8, 0x58, 0x60,
         0xF2, 0x6C, 0x3B, 0x71, 0xD6, 0xE9, 0x7E, 0x79, 0xD2, 0x30, 0x5A, 0x16, 0x1A, 0x53, 0x83,
         0x5C, 0x06, 0x2C, 0x8A, 0xBA, 0x7A, 0x3A, 0xB8>>

  @f15 <<0x1E, 0x00, 0xF2, 0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C,
         0xCA, 0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5, 0x73,
         0x5F, 0x9D, 0xBB, 0xE1, 0xF4, 0x53, 0xCF, 0x8D, 0x45, 0x04, 0xBB, 0xAD, 0x39, 0x89, 0x93,
         0x0C, 0x99, 0x83, 0x67, 0xF7, 0x40, 0x4D, 0x26>>

  @frames [@f1, @f2, @f3, @f4, @f5, @f6, @f7, @f8, @f9, @f10, @f11, @f12, @f13, @f15]

  # The other node's Ed25519 identity, appearing verbatim at offset 2 of every ADVERT.
  @public_key <<0x5E, 0xD2, 0x93, 0xBE, 0x81, 0x22, 0xCB, 0xA3, 0x0D, 0xF6, 0xF6, 0x8C, 0xCA,
                0xDD, 0x32, 0x2A, 0x15, 0x7B, 0xB3, 0xDC, 0x88, 0x40, 0x58, 0xF4, 0xA8, 0xD5,
                0x73, 0x5F, 0x9D, 0xBB, 0xE1, 0xF4>>

  test "every captured frame round-trips through parse/serialize byte-for-byte" do
    for wire <- @frames do
      {:ok, pkt} = :meshcore_protocol.parse(wire)
      assert :meshcore_protocol.serialize(pkt) == wire
    end
  end

  test "a malformed (too short) frame is rejected" do
    assert :meshcore_protocol.parse(<<>>) == {:error, :failed_meshcore_parse}
  end

  test "a non-zero version frame is rejected (meshtastic broadcast, on-air capture)" do
    meshtastic_text =
      <<255, 255, 255, 255, 132, 70, 49, 67, 225, 19, 88, 159, 99, 31, 0, 132, 44, 85, 219, 122,
        143, 81, 66, 98, 85, 230>>

    assert :meshcore_protocol.parse(meshtastic_text) == {:error, :failed_meshcore_parse}
  end

  test "a reserved path hash size (0b11) is rejected" do
    # header version 0 / advert / flood, path_len byte 0b11_000000
    assert :meshcore_protocol.parse(<<0x11, 0xC0>>) == {:error, :failed_meshcore_parse}
  end

  test "packet_hash ignores route and path but tracks the payload" do
    <<_header, _path_len, payload::binary>> = @f8
    {:ok, flood} = :meshcore_protocol.parse(@f8)
    {:ok, direct} = :meshcore_protocol.parse(<<0x16, 0x01, 0xAB, payload::binary>>)
    {:ok, other} = :meshcore_protocol.parse(@f10)

    assert byte_size(:meshcore_protocol.packet_hash(flood)) == 8
    assert :meshcore_protocol.packet_hash(direct) == :meshcore_protocol.packet_hash(flood)
    refute :meshcore_protocol.packet_hash(other) == :meshcore_protocol.packet_hash(flood)
  end

  test "packet_hash matches between a built packet and its parsed wire form" do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    appdata = :meshcore_protocol.encode_advert_appdata(%{node_type: :chat, name: "t"})

    built =
      :meshcore_protocol.sign_advert(
        %{
          route: :flood,
          type: :advert,
          version: 0,
          hash_size: 1,
          path: <<>>,
          public_key: pub,
          timestamp: 123,
          appdata: appdata
        },
        priv
      )

    {:ok, parsed} = :meshcore_protocol.parse(:meshcore_protocol.serialize(built))
    assert :meshcore_protocol.packet_hash(parsed) == :meshcore_protocol.packet_hash(built)
  end

  test "CONTROL discover_req decodes route, sub_type, type_filter and tag" do
    {:ok, pkt} = :meshcore_protocol.parse(@f1)
    assert pkt.route == :direct
    assert pkt.type == :control
    assert pkt.version == 0
    assert pkt.path == <<>>
    assert pkt.sub_type == :discover_req
    assert pkt.prefix_only == false
    assert pkt.type_filter == 0xFF
    assert pkt.tag == <<0xBC, 0xC7, 0x32, 0x10>>
    refute Map.has_key?(pkt, :since)

    {:ok, pkt7} = :meshcore_protocol.parse(@f7)
    assert pkt7.tag == <<0x59, 0x77, 0xC8, 0x7F>>
  end

  test "ADVERT decodes identity, node type and name; routes mark local vs broadcast" do
    {:ok, adv} = :meshcore_protocol.parse(@f4)
    assert adv.type == :advert
    assert adv.route == :direct
    assert adv.version == 0
    assert adv.public_key == @public_key
    assert adv.node_type == :chat
    assert adv.name == "testd"
    assert adv.path == <<>>

    {:ok, bcast} = :meshcore_protocol.parse(@f6)
    assert bcast.route == :flood
    assert bcast.public_key == @public_key
    assert bcast.name == "testd"

    {:ok, renamed} = :meshcore_protocol.parse(@f9)
    assert renamed.name == "testtest"
  end

  test "verify_advert accepts a genuine signature and rejects a tampered one" do
    {:ok, adv} = :meshcore_protocol.parse(@f4)
    assert :meshcore_protocol.verify_advert(adv)

    <<first, rest::binary>> = adv.signature
    tampered = %{adv | signature: <<rem(first + 1, 256), rest::binary>>}
    refute :meshcore_protocol.verify_advert(tampered)
  end

  test "sign_advert produces a signature verify_advert accepts; tampering breaks it" do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    appdata = :meshcore_protocol.encode_advert_appdata(%{node_type: :chat, name: "pocketOS T1"})

    base = %{
      route: :flood,
      type: :advert,
      version: 0,
      hash_size: 1,
      path: <<>>,
      public_key: pub,
      timestamp: 1_700_000_000,
      appdata: appdata
    }

    signed = :meshcore_protocol.sign_advert(base, priv)
    assert byte_size(signed.signature) == 64
    assert :meshcore_protocol.verify_advert(signed)

    <<first, rest::binary>> = signed.signature
    tampered = %{signed | signature: <<rem(first + 1, 256), rest::binary>>}
    refute :meshcore_protocol.verify_advert(tampered)
  end

  test "a self-built signed advert round-trips through serialize/parse" do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    appdata = :meshcore_protocol.encode_advert_appdata(%{node_type: :chat, name: "pocketOS T1"})

    signed =
      :meshcore_protocol.sign_advert(
        %{
          route: :flood,
          type: :advert,
          version: 0,
          hash_size: 1,
          path: <<>>,
          public_key: pub,
          timestamp: 1_700_000_123,
          appdata: appdata
        },
        priv
      )

    wire = :meshcore_protocol.serialize(signed)
    {:ok, adv} = :meshcore_protocol.parse(wire)
    assert adv.route == :flood
    assert adv.type == :advert
    assert adv.flags == 0x81
    assert adv.node_type == :chat
    assert adv.name == "pocketOS T1"
    assert adv.public_key == pub
    assert adv.timestamp == 1_700_000_123
    assert :meshcore_protocol.verify_advert(adv)
  end

  test "GRP_TXT decrypts to the captured plaintext on the Public channel" do
    assert :meshcore_protocol.channel_hash(:meshcore_protocol.default_public_channel_key()) ==
             0x11

    expected = [
      {@f8, "testd: hello"},
      {@f10, "testtest: ciao"},
      {@f13, "testtest: this is a longer message"}
    ]

    for {wire, text} <- expected do
      {:ok, pkt} = :meshcore_protocol.parse(wire)
      assert pkt.type == :grp_txt
      assert pkt.channel_hash == 0x11
      {:ok, dec} = :meshcore_protocol.decrypt(pkt)
      assert dec.text == text
      assert dec.txt_type == 0
      assert dec.attempt == 0
      refute Map.has_key?(dec, :ciphertext)
    end
  end

  test "decrypt rejects a frame whose MAC does not match the key" do
    {:ok, pkt} = :meshcore_protocol.parse(@f8)
    assert :meshcore_protocol.decrypt(pkt, :crypto.strong_rand_bytes(16)) == {:error, :bad_mac}
  end

  test "encrypt/decrypt round-trips a group message and survives a wire round-trip" do
    pkt = %{
      route: :flood,
      type: :grp_txt,
      version: 0,
      hash_size: 1,
      path: <<>>,
      timestamp: 1_700_000_000,
      txt_type: 0,
      attempt: 0,
      text: "pocketOS: hi"
    }

    enc = :meshcore_protocol.encrypt(pkt)
    assert enc.channel_hash == 0x11
    refute Map.has_key?(enc, :text)

    {:ok, dec} = :meshcore_protocol.decrypt(enc)
    assert dec.text == "pocketOS: hi"
    assert dec.timestamp == 1_700_000_000

    {:ok, reparsed} = :meshcore_protocol.parse(:meshcore_protocol.serialize(enc))
    {:ok, redec} = :meshcore_protocol.decrypt(reparsed)
    assert redec.text == "pocketOS: hi"
  end

  test "DISCOVER_RESP serializes to the wire layout and round-trips (full key)" do
    tag = <<0xBC, 0xC7, 0x32, 0x10>>

    resp = %{
      route: :direct,
      type: :control,
      version: 0,
      hash_size: 1,
      path: <<>>,
      sub_type: :discover_resp,
      node_type: :chat,
      reported_snr: 24,
      tag: tag,
      public_key: @public_key
    }

    # header 0x2E (v0/control/direct), path_len 0x00, flags 0x91 (0x9<<4 | chat),
    # snr byte 24, echoed tag, full 32-byte key.
    wire = :meshcore_protocol.serialize(resp)
    assert wire == <<0x2E, 0x00, 0x91, 0x18>> <> tag <> @public_key

    {:ok, back} = :meshcore_protocol.parse(wire)
    assert back.route == :direct
    assert back.type == :control
    assert back.sub_type == :discover_resp
    assert back.node_type == :chat
    assert back.reported_snr == 24
    assert back.tag == tag
    assert back.public_key == @public_key
  end

  test "DISCOVER_RESP carries an 8-byte key prefix and a negative SNR" do
    tag = <<0x01, 0x02, 0x03, 0x04>>
    prefix = binary_part(@public_key, 0, 8)

    resp = %{
      route: :direct,
      type: :control,
      version: 0,
      hash_size: 1,
      path: <<>>,
      sub_type: :discover_resp,
      node_type: :chat,
      reported_snr: -8,
      tag: tag,
      public_key: prefix
    }

    wire = :meshcore_protocol.serialize(resp)
    assert wire == <<0x2E, 0x00, 0x91, 0xF8>> <> tag <> prefix

    {:ok, back} = :meshcore_protocol.parse(wire)
    assert back.reported_snr == -8
    assert back.public_key == prefix
  end

  test "ANON_REQ splits dest_hash, sender pubkey, cipher_mac and ciphertext" do
    {:ok, pkt} = :meshcore_protocol.parse(@f15)
    assert pkt.type == :anon_req
    assert pkt.route == :direct
    assert pkt.version == 0
    assert pkt.path == <<>>
    assert pkt.dest_hash == 0xF2
    assert pkt.sender_pubkey == @public_key
    assert pkt.cipher_mac == <<0x53, 0xCF>>
    assert byte_size(pkt.ciphertext) == 16
  end

  test "shared_secret yields a 32-byte secret, or an error for a low-order key" do
    {bob_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    {_, alice_priv} = :crypto.generate_key(:eddsa, :ed25519)
    assert {:ok, secret} = :meshcore_protocol.shared_secret(alice_priv, bob_pub)
    assert byte_size(secret) == 32
    assert {:error, _} = :meshcore_protocol.shared_secret(alice_priv, <<0::256>>)
  end

  test "decrypt_shared recovers a PKI message built with the documented scheme" do
    {:ok, secret} = pki_secret()
    {mac, ct} = encrypt_shared(secret, 1_700_000_000, "hello test direct")
    {:ok, dec} = :meshcore_protocol.decrypt_shared(secret, %{cipher_mac: mac, ciphertext: ct})
    assert dec.timestamp == 1_700_000_000
    assert dec.req_data == "hello test direct"
    refute Map.has_key?(dec, :ciphertext)
    refute Map.has_key?(dec, :cipher_mac)
  end

  test "decrypt_shared rejects a tampered MAC" do
    {:ok, secret} = pki_secret()
    {<<first, rest::binary>>, ct} = encrypt_shared(secret, 1, "x")
    bad = <<rem(first + 1, 256), rest::binary>>

    assert :meshcore_protocol.decrypt_shared(secret, %{cipher_mac: bad, ciphertext: ct}) ==
             {:error, :bad_mac}
  end

  test "a direct message round-trips through encrypt_shared/decrypt_shared" do
    {alice_pub, alice_priv} = :crypto.generate_key(:eddsa, :ed25519)
    {bob_pub, bob_priv} = :crypto.generate_key(:eddsa, :ed25519)
    {:ok, secret} = :meshcore_protocol.shared_secret(alice_priv, bob_pub)
    assert {:ok, ^secret} = :meshcore_protocol.shared_secret(bob_priv, alice_pub)

    <<dest_hash, _::binary>> = bob_pub
    <<src_hash, _::binary>> = alice_pub

    sealed =
      :meshcore_protocol.encrypt_shared(secret, %{
        route: :flood,
        type: :txt_msg,
        version: 0,
        hash_size: 1,
        path: <<>>,
        dest_hash: dest_hash,
        src_hash: src_hash,
        timestamp: 1_700_000_123,
        attempt: 1,
        text: "hello test direct"
      })

    refute Map.has_key?(sealed, :text)
    assert byte_size(sealed.ciphertext) == 32

    {:ok, pkt} = :meshcore_protocol.parse(:meshcore_protocol.serialize(sealed))
    {:ok, dec} = :meshcore_protocol.decrypt_shared(secret, pkt)
    assert dec.text == "hello test direct"
    assert dec.timestamp == 1_700_000_123
    assert dec.txt_type == 0
    assert dec.attempt == 1
    refute Map.has_key?(dec, :ciphertext)
  end

  test "decrypt_shared rejects a direct message sealed under another secret" do
    {:ok, secret} = pki_secret()
    sealed = :meshcore_protocol.encrypt_shared(secret, %{type: :txt_msg, text: "x"})

    assert :meshcore_protocol.decrypt_shared(:binary.copy(<<1>>, 32), sealed) ==
             {:error, :bad_mac}
  end

  test "a request round-trips through encrypt_shared/decrypt_shared" do
    {:ok, secret} = pki_secret()

    sealed =
      :meshcore_protocol.encrypt_shared(secret, %{
        type: :req,
        timestamp: 42,
        req_data: <<1, 2, 3>>
      })

    {:ok, dec} = :meshcore_protocol.decrypt_shared(secret, sealed)
    assert dec.timestamp == 42
    assert dec.req_data == <<1, 2, 3>>
  end

  test "ack_payload hashes the message prefix with the sender key" do
    msg = %{timestamp: 1_700_000_123, txt_type: 0, attempt: 1, text: "hello test direct"}

    expected =
      binary_part(
        :crypto.hash(
          :sha256,
          <<1_700_000_123::32-little, 0::6, 1::2, "hello test direct">> <> @public_key
        ),
        0,
        4
      )

    assert :meshcore_protocol.ack_payload(msg, @public_key, 0xAB) == expected <> <<0, 0xAB>>
  end

  test "ack_payload hashes up to the first NUL and exposes the tail attempt byte" do
    msg = %{timestamp: 7, txt_type: 0, attempt: 3, text: "hi" <> <<0, 5>>}

    expected =
      binary_part(:crypto.hash(:sha256, <<7::32-little, 0::6, 3::2, "hi">> <> @public_key), 0, 4)

    assert :meshcore_protocol.ack_payload(msg, @public_key, 0) == expected <> <<5, 0>>
  end

  test "ack_payload of an empty text covers just the prefix" do
    msg = %{timestamp: 7, txt_type: 0, attempt: 0, text: ""}
    expected = binary_part(:crypto.hash(:sha256, <<7::32-little, 0>> <> @public_key), 0, 4)
    assert :meshcore_protocol.ack_payload(msg, @public_key, 0xFF) == expected <> <<0, 0xFF>>
  end

  test "an ACK packet serializes to the raw ack bytes and round-trips" do
    ack = %{
      route: :flood,
      type: :ack,
      version: 0,
      hash_size: 1,
      path: <<>>,
      ack: <<1, 2, 3, 4, 5, 6>>
    }

    wire = :meshcore_protocol.serialize(ack)
    assert wire == <<0x0D, 0x00, 1, 2, 3, 4, 5, 6>>

    {:ok, back} = :meshcore_protocol.parse(wire)
    assert back.type == :ack
    assert back.route == :flood
    assert back.ack == <<1, 2, 3, 4, 5, 6>>
    assert :meshcore_protocol.packet_hash(back) == :meshcore_protocol.packet_hash(ack)
  end

  test "a PATH return seals the echoed route and bundled ack under the shared secret" do
    {:ok, secret} = pki_secret()
    ack = <<1, 2, 3, 4, 5, 6>>

    sealed =
      :meshcore_protocol.encrypt_shared(secret, %{
        route: :flood,
        type: :path,
        version: 0,
        hash_size: 1,
        path: <<>>,
        dest_hash: 0x5E,
        src_hash: 0xF2,
        return_path: <<0xAA, 0xBB>>,
        extra_type: :ack,
        extra: ack
      })

    refute Map.has_key?(sealed, :return_path)
    refute Map.has_key?(sealed, :return_hash_size)
    refute Map.has_key?(sealed, :extra_type)
    refute Map.has_key?(sealed, :extra)

    wire = :meshcore_protocol.serialize(sealed)
    assert <<0x21, 0x00, 0x5E, 0xF2, mac::2-bytes, ct::binary>> = wire
    assert <<^mac::2-bytes, _::binary>> = :crypto.mac(:hmac, :sha256, secret, ct)

    <<aes_key::16-bytes, _::binary>> = secret
    plain = :crypto.crypto_one_time(:aes_128_ecb, aes_key, ct, false)
    assert plain == <<0x02, 0xAA, 0xBB, 3>> <> ack <> <<0::size(6 * 8)>>
  end

  test "the PATH return length byte encodes hash size and hop count" do
    {:ok, secret} = pki_secret()

    inner = fn extra_fields ->
      sealed =
        :meshcore_protocol.encrypt_shared(
          secret,
          Map.merge(%{type: :path, extra_type: :ack, extra: <<9>>}, extra_fields)
        )

      <<aes_key::16-bytes, _::binary>> = secret
      :crypto.crypto_one_time(:aes_128_ecb, aes_key, sealed.ciphertext, false)
    end

    assert <<0x00, 3, 9, _::binary>> = inner.(%{return_path: <<>>})

    assert <<0x41, 0xAA, 0xBB, 3, 9, _::binary>> =
             inner.(%{return_path: <<0xAA, 0xBB>>, return_hash_size: 2})
  end

  test "a PATH return round-trips through encrypt_shared/serialize/parse/decrypt_shared" do
    {:ok, secret} = pki_secret()
    ack = <<1, 2, 3, 4, 5, 6>>

    sealed =
      :meshcore_protocol.encrypt_shared(secret, %{
        route: :flood,
        type: :path,
        version: 0,
        hash_size: 1,
        path: <<>>,
        dest_hash: 0x5E,
        src_hash: 0xF2,
        return_path: <<0xAA, 0xBB>>,
        return_hash_size: 2,
        extra_type: :ack,
        extra: ack
      })

    {:ok, pkt} = :meshcore_protocol.parse(:meshcore_protocol.serialize(sealed))
    {:ok, dec} = :meshcore_protocol.decrypt_shared(secret, pkt)
    assert dec.return_path == <<0xAA, 0xBB>>
    assert dec.return_hash_size == 2
    assert dec.extra_type == :ack
    assert binary_part(dec.extra, 0, 6) == ack
    refute Map.has_key?(dec, :ciphertext)
    refute Map.has_key?(dec, :cipher_mac)
  end

  defp pki_secret do
    {bob_pub, _} = :crypto.generate_key(:eddsa, :ed25519)
    {_, alice_priv} = :crypto.generate_key(:eddsa, :ed25519)
    :meshcore_protocol.shared_secret(alice_priv, bob_pub)
  end

  defp encrypt_shared(secret, timestamp, req_data) do
    inner = <<timestamp::32-little>> <> req_data
    pad = rem(16 - rem(byte_size(inner), 16), 16)
    padded = inner <> :binary.copy(<<0>>, pad)
    <<aes_key::16-bytes, _::binary>> = secret
    ct = :crypto.crypto_one_time(:aes_128_ecb, aes_key, padded, true)
    <<mac::2-bytes, _::binary>> = :crypto.mac(:hmac, :sha256, secret, ct)
    {mac, ct}
  end
end
