-module(meshtastic).
-export([
    parse/1,
    serialize/1,
    decrypt/1,
    decrypt/2,
    encrypt/1,
    encrypt/2,
    channel_hash/2,
    default_long_fast_psk/0,
    default_long_fast_channel/0
]).

parse(Payload) ->
    case Payload of
        <<DestAddr:32/little-unsigned-integer, SrcAddr:32/little-unsigned-integer,
            PktId:32/little-unsigned-integer, HopStart:3, ViaMqtt:1, WantAck:1, HopLimit:3,
            ChannelHash:8, _Padding:16, Data/binary>> ->
            {ok, #{
                dest => DestAddr,
                src => SrcAddr,
                packet_id => PktId,
                hop_start => HopStart,
                via_mqtt => to_bool(ViaMqtt),
                want_ack => to_bool(WantAck),
                hop_limit => HopLimit,
                channel_hash => ChannelHash,
                encrypted_data => Data
            }};
        _Invalid ->
            {error, failed_meshtastic_parse}
    end.

to_bool(0) -> false;
to_bool(1) -> true.

serialize(#{
    dest := DestAddr,
    src := SrcAddr,
    packet_id := PktId,
    hop_start := HopStart,
    via_mqtt := ViaMqttBool,
    want_ack := WantAckBool,
    hop_limit := HopLimit,
    channel_hash := ChannelHash,
    encrypted_data := Data
}) ->
    ViaMqtt = bool_to_int(ViaMqttBool),
    WantAck = bool_to_int(WantAckBool),
    <<DestAddr:32/little-unsigned-integer, SrcAddr:32/little-unsigned-integer,
        PktId:32/little-unsigned-integer, HopStart:3, ViaMqtt:1, WantAck:1, HopLimit:3,
        ChannelHash:8, 0:16, Data/binary>>.

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.

decrypt(Packet) ->
    decrypt(Packet, default_long_fast_psk()).

encrypt(Packet) ->
    encrypt(Packet, default_long_fast_psk()).

default_long_fast_psk() ->
    <<16#d4, 16#f1, 16#bb, 16#3a, 16#20, 16#29, 16#07, 16#59, 16#f0, 16#bc, 16#ff, 16#ab, 16#cf,
        16#4e, 16#69, 16#01>>.

default_long_fast_channel() ->
    Name = <<"LongFast">>,
    Psk = default_long_fast_psk(),
    #{name => Name, psk => Psk, hash => channel_hash(Name, Psk)}.

channel_hash(Name, Psk) when is_binary(Name), is_binary(Psk) ->
    xor_bytes(Name) bxor xor_bytes(Psk).

xor_bytes(Bin) ->
    xor_bytes(Bin, 0).

xor_bytes(<<>>, Acc) ->
    Acc;
xor_bytes(<<B, Rest/binary>>, Acc) ->
    xor_bytes(Rest, Acc bxor B).

decrypt(Packet, Key) ->
    #{src := SrcAddr, packet_id := PktId, encrypted_data := EncData} = Packet,
    IV = <<PktId:64/little-unsigned, SrcAddr:32/little-unsigned, 0:32>>,
    Decrypted = crypto:crypto_one_time(aes_128_ctr, Key, IV, EncData, false),
    PktWithData = Packet#{data => Decrypted},
    maps:remove(encrypted_data, PktWithData).

encrypt(Packet, Key) ->
    #{src := SrcAddr, packet_id := PktId, data := Data} = Packet,
    IV = <<PktId:64/little-unsigned, SrcAddr:32/little-unsigned, 0:32>>,
    Encrypted = crypto:crypto_one_time(aes_128_ctr, Key, IV, Data, true),
    PktWithEnc = Packet#{encrypted_data => Encrypted},
    maps:remove(data, PktWithEnc).
