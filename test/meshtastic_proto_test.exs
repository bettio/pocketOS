defmodule MeshtasticProtoTest do
  use ExUnit.Case, async: true

  test "an ack's ROUTING payload decodes to a map even when empty/absent" do
    # portnum=ROUTING_APP(5) and request_id=0x99, with no payload field at all:
    # a NONE error_reason is typically omitted on the wire by other encoders
    bare = <<0x08, 0x05, 0x35, 0x99, 0x00, 0x00, 0x00>>
    decoded = :meshtastic_proto.decode(bare)
    assert decoded.portnum == :ROUTING_APP
    assert decoded.request_id == 0x99
    assert decoded.payload == %{}
  end

  test "a ROUTING error_reason round-trips across the error enum" do
    for reason <- [
          :NONE,
          :NO_ROUTE,
          :GOT_NAK,
          :TIMEOUT,
          :MAX_RETRANSMIT,
          :NO_CHANNEL,
          :TOO_LARGE,
          :NO_RESPONSE,
          :DUTY_CYCLE_LIMIT,
          :BAD_REQUEST,
          :PKI_FAILED,
          :PKI_UNKNOWN_PUBKEY
        ] do
      wire =
        %{portnum: :ROUTING_APP, payload: %{error_reason: reason}, request_id: 0x42}
        |> :meshtastic_proto.encode()
        |> :erlang.iolist_to_binary()

      decoded = :meshtastic_proto.decode(wire)
      assert decoded.payload == %{error_reason: reason}
      assert decoded.request_id == 0x42
    end
  end

  test "an unknown error_reason value decodes as its raw integer" do
    # Data payload (field 2) holding a Routing with error_reason (field 3) = 99
    wire = <<0x08, 0x05, 0x12, 0x02, 0x18, 0x63>>
    decoded = :meshtastic_proto.decode(wire)
    assert decoded.payload == %{error_reason: 99}
  end
end
