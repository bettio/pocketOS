defmodule MeshtasticTest do
  use ExUnit.Case

  test "relay_node_byte is the last byte of the node id, with 0 mapped to 0xFF" do
    assert :meshtastic.relay_node_byte(0x43314684) == 0x84
    assert :meshtastic.relay_node_byte(0x00000001) == 0x01
    # 0 is the "unset" sentinel, so a last byte of 0 is remapped to 0xFF
    assert :meshtastic.relay_node_byte(0x12345600) == 0xFF
  end

  test "parse and serialize round-trip next_hop and relay_node" do
    wire =
      <<0xFF, 0xFF, 0xFF, 0xFF, 0x84, 0x46, 0x31, 0x43, 1, 2, 3, 4, 0x68, 8, 0xAB, 0xCD,
        "payload">>

    {:ok, pkt} = :meshtastic.parse(wire)
    assert pkt.next_hop == 0xAB
    assert pkt.relay_node == 0xCD
    assert :meshtastic.serialize(pkt) == wire
  end
end
