-module(meshtastic).
-export([
    parse/1,
    serialize/1,
    decrypt/1,
    decrypt/2,
    encrypt/1,
    encrypt/2,
    decrypt_pki/3,
    encrypt_pki/3,
    channel_hash/2,
    relay_node_byte/1,
    default_long_fast_psk/0,
    default_long_fast_channel/0
]).

parse(Payload) ->
    case Payload of
        <<DestAddr:32/little-unsigned-integer, SrcAddr:32/little-unsigned-integer,
            PktId:32/little-unsigned-integer, HopStart:3, ViaMqtt:1, WantAck:1, HopLimit:3,
            ChannelHash:8, NextHop:8, RelayNode:8, Data/binary>> ->
            {ok, #{
                dest => DestAddr,
                src => SrcAddr,
                packet_id => PktId,
                hop_start => HopStart,
                via_mqtt => to_bool(ViaMqtt),
                want_ack => to_bool(WantAck),
                hop_limit => HopLimit,
                channel_hash => ChannelHash,
                next_hop => NextHop,
                relay_node => RelayNode,
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
    next_hop := NextHop,
    relay_node := RelayNode,
    encrypted_data := Data
}) ->
    ViaMqtt = bool_to_int(ViaMqttBool),
    WantAck = bool_to_int(WantAckBool),
    <<DestAddr:32/little-unsigned-integer, SrcAddr:32/little-unsigned-integer,
        PktId:32/little-unsigned-integer, HopStart:3, ViaMqtt:1, WantAck:1, HopLimit:3,
        ChannelHash:8, NextHop:8, RelayNode:8, Data/binary>>.

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.

relay_node_byte(NodeNum) ->
    case NodeNum band 16#FF of
        0 -> 16#FF;
        Byte -> Byte
    end.

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

decrypt_pki(Packet, OurPriv, PeerPub) ->
    #{src := SrcAddr, packet_id := PktId, encrypted_data := EncData} = Packet,
    case EncData of
        <<_/binary>> when byte_size(EncData) >= 12 ->
            CipherLen = byte_size(EncData) - 12,
            <<Ciphertext:CipherLen/binary, Tag:8/binary, ExtraNonce:32/little-unsigned>> = EncData,
            Shared = crypto:compute_key(eddh, PeerPub, OurPriv, x25519),
            Key = crypto:hash(sha256, Shared),
            Nonce = <<PktId:32/little-unsigned, ExtraNonce:32/little-unsigned,
                SrcAddr:32/little-unsigned, 0:8>>,
            case crypto:crypto_one_time_aead(aes_256_ccm, Key, Nonce, Ciphertext, <<>>, Tag, false) of
                error ->
                    {error, pki_decrypt_failed};
                Plain when is_binary(Plain) ->
                    PktWithData = Packet#{data => Plain},
                    {ok, maps:remove(encrypted_data, PktWithData)}
            end;
        _ ->
            {error, pki_payload_too_short}
    end.

encrypt_pki(Packet, OurPriv, PeerPub) ->
    #{src := SrcAddr, packet_id := PktId, data := Data} = Packet,
    ExtraNonce = decode_unsigned(crypto:strong_rand_bytes(4), little),
    Shared = crypto:compute_key(eddh, PeerPub, OurPriv, x25519),
    Key = crypto:hash(sha256, Shared),
    Nonce = <<PktId:32/little-unsigned, ExtraNonce:32/little-unsigned, SrcAddr:32/little-unsigned,
        0:8>>,
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(aes_256_ccm, Key, Nonce, Data, <<>>, 8, true),
    EncData = <<Ciphertext/binary, Tag/binary, ExtraNonce:32/little-unsigned>>,
    PktWithEnc = Packet#{encrypted_data => EncData},
    maps:remove(data, PktWithEnc).

%% TODO: drop once AtomVM exposes binary:decode_unsigned/2.
%% Pure-Erlang reimplementation of binary:decode_unsigned/2 (big and little
%% endianness), folding one byte at a time so it stays AtomVM-compatible.
decode_unsigned(Bin, big) ->
    decode_unsigned_big(Bin, 0);
decode_unsigned(Bin, little) ->
    decode_unsigned_little(Bin, 0, 0).

decode_unsigned_big(<<>>, Acc) ->
    Acc;
decode_unsigned_big(<<Byte, Rest/binary>>, Acc) ->
    decode_unsigned_big(Rest, (Acc bsl 8) bor Byte).

decode_unsigned_little(<<>>, _Shift, Acc) ->
    Acc;
decode_unsigned_little(<<Byte, Rest/binary>>, Shift, Acc) ->
    decode_unsigned_little(Rest, Shift + 8, Acc bor (Byte bsl Shift)).
