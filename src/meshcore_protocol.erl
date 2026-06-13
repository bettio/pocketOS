-module(meshcore_protocol).

-export([
    parse/1,
    serialize/1,
    packet_hash/1,
    decrypt/1,
    decrypt/2,
    decrypt_shared/2,
    encrypt_shared/2,
    shared_secret/2,
    ack_payload/3,
    encrypt/1,
    encrypt/2,
    verify_advert/1,
    sign_advert/2,
    encode_advert_appdata/1,
    eddsa_available/0,
    channel_hash/1,
    default_public_channel/0,
    default_public_channel_key/0,
    node_type_to_int/1
]).

-type packet() :: map().

%% === parse ===

%% Parse a raw MeshCore frame into a map: header, optional transport codes, path,
%% and the typed payload for known types (unknown types keep a raw `payload').
-spec parse(binary()) -> {ok, packet()} | {error, atom()}.
%% Only version 0 (PAYLOAD_VER_1) exists on-air; reject anything else.
parse(<<0:2, TypeInt:4, RouteInt:2, Rest/binary>>) ->
    parse_frame(int_to_route(RouteInt), int_to_type(TypeInt), 0, Rest);
parse(_Invalid) ->
    {error, failed_meshcore_parse}.

parse_frame(Route, Type, Version, Rest0) ->
    case take_transport_codes(Route, Rest0) of
        {ok, _TransportCodes, <<3:2, _:6, _/binary>>} ->
            {error, failed_meshcore_parse};
        {ok, TransportCodes, <<HashSizeCode:2, HopCount:6, Rest1/binary>>} ->
            HashSize = HashSizeCode + 1,
            PathLen = HopCount * HashSize,
            case Rest1 of
                <<Path:PathLen/binary, Payload/binary>> ->
                    Base = base_map(Route, Type, Version, HashSize, Path, TransportCodes),
                    {ok, parse_payload(Type, Payload, Base)};
                _ ->
                    {error, failed_meshcore_parse}
            end;
        _ ->
            {error, failed_meshcore_parse}
    end.

take_transport_codes(Route, Rest) when Route =:= transport_flood; Route =:= transport_direct ->
    case Rest of
        <<C1:16/little-unsigned, C2:16/little-unsigned, After/binary>> ->
            {ok, #{transport_code_1 => C1, transport_code_2 => C2}, After};
        _ ->
            error
    end;
take_transport_codes(_Route, Rest) ->
    {ok, none, Rest}.

base_map(Route, Type, Version, HashSize, Path, none) ->
    #{route => Route, type => Type, version => Version, hash_size => HashSize, path => Path};
base_map(Route, Type, Version, HashSize, Path, #{transport_code_1 := C1, transport_code_2 := C2}) ->
    #{
        route => Route,
        type => Type,
        version => Version,
        hash_size => HashSize,
        path => Path,
        transport_code_1 => C1,
        transport_code_2 => C2
    }.

parse_payload(advert, Payload, Base) ->
    case Payload of
        <<PubKey:32/binary, Timestamp:32/little-unsigned, Signature:64/binary, AppData/binary>> ->
            Adv = Base#{
                public_key => PubKey,
                timestamp => Timestamp,
                signature => Signature,
                appdata => AppData
            },
            parse_advert_appdata(AppData, Adv);
        _ ->
            Base#{payload => Payload}
    end;
parse_payload(control, <<SubType:4, Lo:4, Data/binary>> = Payload, Base) ->
    parse_control(SubType, Lo, Data, Payload, Base);
parse_payload(Type, Payload, Base) when Type =:= grp_txt; Type =:= grp_data ->
    case Payload of
        <<ChannelHash, CipherMac:2/binary, Ciphertext/binary>> ->
            Base#{channel_hash => ChannelHash, cipher_mac => CipherMac, ciphertext => Ciphertext};
        _ ->
            Base#{payload => Payload}
    end;
parse_payload(Type, Payload, Base) when
    Type =:= txt_msg; Type =:= req; Type =:= response; Type =:= path
->
    case Payload of
        <<DestHash, SrcHash, CipherMac:2/binary, Ciphertext/binary>> ->
            Base#{
                dest_hash => DestHash,
                src_hash => SrcHash,
                cipher_mac => CipherMac,
                ciphertext => Ciphertext
            };
        _ ->
            Base#{payload => Payload}
    end;
parse_payload(anon_req, Payload, Base) ->
    case Payload of
        <<DestHash, SenderPubKey:32/binary, CipherMac:2/binary, Ciphertext/binary>> ->
            Base#{
                dest_hash => DestHash,
                sender_pubkey => SenderPubKey,
                cipher_mac => CipherMac,
                ciphertext => Ciphertext
            };
        _ ->
            Base#{payload => Payload}
    end;
parse_payload(ack, Payload, Base) ->
    Base#{ack => Payload};
parse_payload(_Type, Payload, Base) ->
    Base#{payload => Payload}.

%% advert appdata
parse_advert_appdata(<<>>, Adv) ->
    Adv;
parse_advert_appdata(<<Flags, Rest0/binary>>, Adv0) ->
    <<HasName:1, HasFeat2:1, HasFeat1:1, HasLoc:1, NodeType:4>> = <<Flags>>,
    Adv1 = Adv0#{flags => Flags, node_type => node_type(NodeType)},
    {Adv2, Rest1} = take_advert_location(HasLoc, Rest0, Adv1),
    {Adv3, Rest2} = take_advert_feature(HasFeat1, feature_1, Rest1, Adv2),
    {Adv4, Rest3} = take_advert_feature(HasFeat2, feature_2, Rest2, Adv3),
    take_advert_name(HasName, Rest3, Adv4).

take_advert_location(1, <<Lat:32/little-signed, Lon:32/little-signed, After/binary>>, Adv) ->
    {Adv#{latitude => Lat, longitude => Lon}, After};
take_advert_location(_HasLoc, Rest, Adv) ->
    {Adv, Rest}.

take_advert_feature(1, Key, <<Feature:16/little-unsigned, After/binary>>, Adv) ->
    {Adv#{Key => Feature}, After};
take_advert_feature(_HasFeat, _Key, Rest, Adv) ->
    {Adv, Rest}.

take_advert_name(1, Name, Adv) ->
    Adv#{name => Name};
take_advert_name(_HasName, _Name, Adv) ->
    Adv.

%% Build advert appdata: flags byte (node type | name-present) ++ name. Minimal
%% inverse of parse_advert_appdata/2 -- node type and name only (no location or
%% feature fields), which is all our own adverts carry.
-spec encode_advert_appdata(map()) -> binary().
encode_advert_appdata(#{node_type := NodeType, name := Name}) ->
    <<1:1, 0:3, (node_type_to_int(NodeType)):4, Name/binary>>.

%% control / discovery -- match the body shape in the head; Lo is the flags low nibble
parse_control(16#8, Lo, <<TypeFilter, Tag:4/binary>>, _Payload, Base) ->
    Base#{
        sub_type => discover_req,
        prefix_only => (Lo band 1) =:= 1,
        type_filter => TypeFilter,
        tag => Tag
    };
parse_control(16#8, Lo, <<TypeFilter, Tag:4/binary, Since:32/little-unsigned>>, _Payload, Base) ->
    Base#{
        sub_type => discover_req,
        prefix_only => (Lo band 1) =:= 1,
        type_filter => TypeFilter,
        tag => Tag,
        since => Since
    };
parse_control(16#9, NodeType, <<Snr:8/signed, Tag:4/binary, Key/binary>>, _Payload, Base) ->
    Base#{
        sub_type => discover_resp,
        node_type => node_type(NodeType),
        reported_snr => Snr,
        tag => Tag,
        public_key => Key
    };
parse_control(SubType, _Flags, _Data, Payload, Base) ->
    Base#{sub_type => {raw, SubType}, payload => Payload}.

%% === serialize ===

%% Serialize a parsed map back to a raw frame. Byte-exact inverse of parse/1
%% (adverts preserve the captured signature -- we never re-sign here).
-spec serialize(packet()) -> binary().
serialize(
    #{route := Route, type := Type, version := Version, hash_size := HashSize, path := Path} = Packet
) ->
    RouteInt = route_to_int(Route),
    TypeInt = type_to_int(Type),
    Transport = serialize_transport(Route, Packet),
    HopCount =
        case HashSize of
            0 -> 0;
            _ -> byte_size(Path) div HashSize
        end,
    Payload = serialize_payload(Type, Packet),
    <<Version:2, TypeInt:4, RouteInt:2, Transport/binary, (HashSize - 1):2, HopCount:6, Path/binary,
        Payload/binary>>.

serialize_transport(Route, Packet) when Route =:= transport_flood; Route =:= transport_direct ->
    C1 = maps:get(transport_code_1, Packet, 0),
    C2 = maps:get(transport_code_2, Packet, 0),
    <<C1:16/little-unsigned, C2:16/little-unsigned>>;
serialize_transport(_Route, _Packet) ->
    <<>>.

serialize_payload(advert, Packet) ->
    #{public_key := PubKey, timestamp := Timestamp, signature := Signature} = Packet,
    AppData = maps:get(appdata, Packet, <<>>),
    <<PubKey/binary, Timestamp:32/little-unsigned, Signature/binary, AppData/binary>>;
serialize_payload(control, #{sub_type := discover_req} = Packet) ->
    #{prefix_only := PrefixOnly, type_filter := TypeFilter, tag := Tag} = Packet,
    Since =
        case maps:get(since, Packet, undefined) of
            undefined -> <<>>;
            S -> <<S:32/little-unsigned>>
        end,
    <<16#8:4, 0:3, (bool_to_int(PrefixOnly)):1, TypeFilter, Tag/binary, Since/binary>>;
serialize_payload(control, #{sub_type := discover_resp} = Packet) ->
    #{node_type := NodeType, reported_snr := Snr, tag := Tag, public_key := Key} = Packet,
    <<16#9:4, (node_type_to_int(NodeType)):4, Snr:8/signed, Tag/binary, Key/binary>>;
serialize_payload(Type, #{channel_hash := ChannelHash, cipher_mac := Mac, ciphertext := CT}) when
    Type =:= grp_txt; Type =:= grp_data
->
    <<ChannelHash, Mac/binary, CT/binary>>;
serialize_payload(Type, #{dest_hash := Dest, src_hash := Src, cipher_mac := Mac, ciphertext := CT}) when
    Type =:= txt_msg; Type =:= req; Type =:= response; Type =:= path
->
    <<Dest, Src, Mac/binary, CT/binary>>;
serialize_payload(anon_req, #{
    dest_hash := Dest, sender_pubkey := Sender, cipher_mac := Mac, ciphertext := CT
}) ->
    <<Dest, Sender/binary, Mac/binary, CT/binary>>;
serialize_payload(ack, #{ack := Ack}) ->
    Ack;
serialize_payload(_Type, #{payload := Payload}) ->
    Payload.

%% === packet hash ===

%% 8-byte dedup hash over the payload-type byte and the payload bytes. Route
%% and path are excluded, so the same packet heard via another route matches.
-spec packet_hash(packet()) -> binary().
packet_hash(#{type := Type} = Packet) ->
    Payload =
        case Packet of
            #{payload := Raw} -> Raw;
            _ -> serialize_payload(Type, Packet)
        end,
    <<Hash:8/binary, _/binary>> = crypto:hash(sha256, <<(type_to_int(Type)), Payload/binary>>),
    Hash.

%% === decrypt ===

%% Decrypt a parsed group-text packet with the default Public channel key.
-spec decrypt(packet()) -> {ok, packet()} | {error, atom()}.
decrypt(Packet) ->
    decrypt(Packet, default_public_channel_key()).

%% Verify the 2-byte HMAC then AES-128-ECB-decrypt, yielding the inner
%% timestamp / txt_type / attempt / text (trailing zero padding stripped).
-spec decrypt(packet(), binary()) -> {ok, packet()} | {error, atom()}.
decrypt(#{cipher_mac := Mac, ciphertext := CT} = Packet, Key) ->
    <<CalcMac:2/binary, _/binary>> = crypto:mac(hmac, sha256, Key, CT),
    case CalcMac =:= Mac of
        false -> {error, bad_mac};
        true -> parse_msg_inner(aes_ecb(Key, CT, false), Packet)
    end;
decrypt(_Packet, _Key) ->
    {error, no_ciphertext}.

parse_msg_inner(<<Timestamp:32/little-unsigned, TxtType:6, Attempt:2, Text/binary>>, Packet) ->
    {ok,
        without_cipher(Packet#{
            timestamp => Timestamp,
            txt_type => TxtType,
            attempt => Attempt,
            text => strip_trailing_nuls(Text)
        })};
parse_msg_inner(_Plain, _Packet) ->
    {error, short_plaintext}.

without_cipher(Packet) ->
    maps:remove(cipher_mac, maps:remove(ciphertext, Packet)).

strip_trailing_nuls(Bin) ->
    Len = last_nonzero(Bin, 0, 0),
    <<Prefix:Len/binary, _/binary>> = Bin,
    Prefix.

last_nonzero(<<>>, _Index, LastLen) ->
    LastLen;
last_nonzero(<<0, Rest/binary>>, Index, LastLen) ->
    last_nonzero(Rest, Index + 1, LastLen);
last_nonzero(<<_Byte, Rest/binary>>, Index, _LastLen) ->
    last_nonzero(Rest, Index + 1, Index + 1).

%% === shared-secret (PKI) ===

%% X25519 ECDH shared secret from our Ed25519 seed and a peer's Ed25519
%% public key. A bad/low-order peer key comes back as {error, Reason}.
-spec shared_secret(binary(), binary()) -> {ok, binary()} | {error, term()}.
shared_secret(OurSeed, PeerEdPub) ->
    try
        {ok, ed25519_x25519:shared_secret(OurSeed, PeerEdPub)}
    catch
        error:Reason -> {error, Reason}
    end.

%% Decrypt a PKI message with a 32-byte shared secret (HMAC on the full
%% secret, AES key = its first 16 bytes). A txt_msg yields the text-message
%% inner fields; a path yields the returned route plus its bundled extra;
%% request-style payloads yield timestamp + req_data.
-spec decrypt_shared(binary(), packet()) -> {ok, packet()} | {error, atom()}.
decrypt_shared(Secret, #{cipher_mac := Mac, ciphertext := CT} = Packet) when
    byte_size(Secret) =:= 32
->
    <<CalcMac:2/binary, _/binary>> = crypto:mac(hmac, sha256, Secret, CT),
    case CalcMac =:= Mac of
        false ->
            {error, bad_mac};
        true ->
            <<AesKey:16/binary, _/binary>> = Secret,
            parse_shared_inner(aes_ecb(AesKey, CT, false), Packet)
    end;
decrypt_shared(_Secret, _Packet) ->
    {error, no_ciphertext}.

parse_shared_inner(Plain, #{type := txt_msg} = Packet) ->
    parse_msg_inner(Plain, Packet);
parse_shared_inner(<<HashSizeCode:2, HopCount:6, Rest/binary>>, #{type := path} = Packet) ->
    HashSize = HashSizeCode + 1,
    PathLen = HopCount * HashSize,
    case Rest of
        <<ReturnPath:PathLen/binary, ExtraTypeInt, Extra/binary>> ->
            {ok,
                without_cipher(Packet#{
                    return_hash_size => HashSize,
                    return_path => ReturnPath,
                    extra_type => int_to_type(ExtraTypeInt),
                    extra => Extra
                })};
        _ ->
            {error, short_plaintext}
    end;
parse_shared_inner(<<Timestamp:32/little-unsigned, ReqData/binary>>, Packet) ->
    {ok,
        without_cipher(Packet#{timestamp => Timestamp, req_data => strip_trailing_nuls(ReqData)})};
parse_shared_inner(_Plain, _Packet) ->
    {error, short_plaintext}.

%% Encrypt a PKI packet with a 32-byte shared secret -- inverse of
%% decrypt_shared/2. A txt_msg seals the text-message inner fields; a path
%% packet seals the returned route plus its bundled extra; request-style
%% packets seal timestamp + req_data.
-spec encrypt_shared(binary(), packet()) -> packet().
encrypt_shared(Secret, #{type := txt_msg, text := Text} = Packet) when byte_size(Secret) =:= 32 ->
    Timestamp = maps:get(timestamp, Packet, 0),
    TxtType = maps:get(txt_type, Packet, 0),
    Attempt = maps:get(attempt, Packet, 0),
    Inner = <<Timestamp:32/little-unsigned, TxtType:6, Attempt:2, Text/binary>>,
    seal_shared(Secret, Inner, maps:remove(text, Packet));
encrypt_shared(
    Secret, #{type := path, return_path := Path, extra_type := ExtraType, extra := Extra} = Packet
) when byte_size(Secret) =:= 32 ->
    HashSize = maps:get(return_hash_size, Packet, 1),
    Inner =
        <<(HashSize - 1):2, (byte_size(Path) div HashSize):6, Path/binary,
            (type_to_int(ExtraType)), Extra/binary>>,
    seal_shared(Secret, Inner, without_path_return(Packet));
encrypt_shared(Secret, #{req_data := ReqData} = Packet) when byte_size(Secret) =:= 32 ->
    Timestamp = maps:get(timestamp, Packet, 0),
    Inner = <<Timestamp:32/little-unsigned, ReqData/binary>>,
    seal_shared(Secret, Inner, maps:remove(req_data, Packet)).

seal_shared(Secret, Inner, Packet) ->
    <<AesKey:16/binary, _/binary>> = Secret,
    CT = aes_ecb(AesKey, pad_to_block(Inner, 16), true),
    <<Mac:2/binary, _/binary>> = crypto:mac(hmac, sha256, Secret, CT),
    Packet#{cipher_mac => Mac, ciphertext => CT}.

without_path_return(Packet) ->
    P1 = maps:remove(return_path, maps:remove(return_hash_size, Packet)),
    maps:remove(extra_type, maps:remove(extra, P1)).

%% 6-byte acknowledgement body for a decrypted plain text message: a truncated
%% hash over the message prefix and the sender key, the extended-attempt byte,
%% and a caller-supplied random byte. Receivers match the first 4 bytes only.
-spec ack_payload(packet(), binary(), byte()) -> binary().
ack_payload(
    #{timestamp := Ts, txt_type := TxtType, attempt := Attempt, text := Text}, SenderPub, RandByte
) ->
    Len = text_len(Text),
    <<Visible:Len/binary, Tail/binary>> = Text,
    Prefix = <<Ts:32/little-unsigned, TxtType:6, Attempt:2, Visible/binary>>,
    <<Hash:4/binary, _/binary>> = crypto:hash(sha256, <<Prefix/binary, SenderPub/binary>>),
    <<Hash/binary, (attempt_byte(Tail)), RandByte>>.

text_len(Bin) -> text_len(Bin, 0).

text_len(<<0, _/binary>>, Len) -> Len;
text_len(<<_, Rest/binary>>, Len) -> text_len(Rest, Len + 1);
text_len(<<>>, Len) -> Len.

%% The byte after the text's NUL carries the attempt number past the 2-bit field.
attempt_byte(<<0, Byte, _/binary>>) -> Byte;
attempt_byte(_Tail) -> 0.

%% === encrypt ===

%% Encrypt a group-text packet (`text', optional `timestamp'/`txt_type'/
%% `attempt') with the default Public channel key.
-spec encrypt(packet()) -> packet().
encrypt(Packet) ->
    encrypt(Packet, default_public_channel_key()).

-spec encrypt(packet(), binary()) -> packet().
encrypt(#{text := Text} = Packet, Key) ->
    Timestamp = maps:get(timestamp, Packet, 0),
    TxtType = maps:get(txt_type, Packet, 0),
    Attempt = maps:get(attempt, Packet, 0),
    Inner = <<Timestamp:32/little-unsigned, TxtType:6, Attempt:2, Text/binary>>,
    Padded = pad_to_block(Inner, 16),
    CT = aes_ecb(Key, Padded, true),
    <<Mac:2/binary, _/binary>> = crypto:mac(hmac, sha256, Key, CT),
    Encrypted = Packet#{channel_hash => channel_hash(Key), cipher_mac => Mac, ciphertext => CT},
    maps:remove(text, Encrypted).

pad_to_block(Bin, Block) ->
    case byte_size(Bin) rem Block of
        0 -> Bin;
        Rem -> <<Bin/binary, 0:((Block - Rem) * 8)>>
    end.

%% === channel ===

%% First byte of SHA-256(channel key) -- the on-wire channel selector.
-spec channel_hash(binary()) -> byte().
channel_hash(Key) ->
    <<First, _/binary>> = crypto:hash(sha256, Key),
    First.

%% The well-known MeshCore "Public" channel key (16 bytes).
-spec default_public_channel_key() -> binary().
default_public_channel_key() ->
    <<16#8b, 16#33, 16#87, 16#e9, 16#c5, 16#cd, 16#ea, 16#6a, 16#c9, 16#e5, 16#ed, 16#ba, 16#a1,
        16#15, 16#cd, 16#72>>.

-spec default_public_channel() -> map().
default_public_channel() ->
    Key = default_public_channel_key(),
    #{name => <<"Public">>, key => Key, hash => channel_hash(Key)}.

%% === advert signature ===

%% Ed25519-verify an advert's signature over public_key||timestamp||appdata.
%% Returns false (never crashes) where eddsa is unavailable -- best-effort.
-spec verify_advert(packet()) -> boolean().
verify_advert(#{public_key := PubKey, timestamp := Timestamp, signature := Sig, appdata := AppData}) ->
    Msg = <<PubKey/binary, Timestamp:32/little-unsigned, AppData/binary>>,
    try
        crypto:verify(eddsa, none, Msg, Sig, [PubKey, ed25519])
    catch
        _:_ -> false
    end;
verify_advert(_Packet) ->
    false.

%% Ed25519-sign an advert over public_key||timestamp||appdata, returning the
%% packet with the `signature' filled in. Inverse of verify_advert/1; the signed
%% bytes must stay byte-identical to it. Requires eddsa_available/0.
-spec sign_advert(packet(), binary()) -> packet().
sign_advert(#{public_key := PubKey, timestamp := Timestamp} = Packet, PrivKey) ->
    AppData = maps:get(appdata, Packet, <<>>),
    Msg = <<PubKey/binary, Timestamp:32/little-unsigned, AppData/binary>>,
    Packet#{signature => crypto:sign(eddsa, none, Msg, [PrivKey, ed25519])}.

%% Whether this runtime can do Ed25519 (needs libsodium; OpenSSL on the host).
%% Advert verification and the PKI direct-message path depend on it.
-spec eddsa_available() -> boolean().
eddsa_available() ->
    has_eddsa_backend(crypto:info_lib()).

has_eddsa_backend([]) -> false;
has_eddsa_backend([{<<"OpenSSL">>, _, _} | _]) -> true;
has_eddsa_backend([{<<"libsodium">>, _, _} | _]) -> true;
has_eddsa_backend([_ | T]) -> has_eddsa_backend(T).

%% === shared helpers ===

aes_ecb(Key, Data, Encrypt) ->
    State = crypto:crypto_init(aes_128_ecb, Key, <<>>, [{encrypt, Encrypt}]),
    Head = crypto:crypto_update(State, Data),
    Tail = crypto:crypto_final(State),
    <<Head/binary, Tail/binary>>.

int_to_route(0) -> transport_flood;
int_to_route(1) -> flood;
int_to_route(2) -> direct;
int_to_route(3) -> transport_direct.

route_to_int(transport_flood) -> 0;
route_to_int(flood) -> 1;
route_to_int(direct) -> 2;
route_to_int(transport_direct) -> 3.

int_to_type(0) -> req;
int_to_type(1) -> response;
int_to_type(2) -> txt_msg;
int_to_type(3) -> ack;
int_to_type(4) -> advert;
int_to_type(5) -> grp_txt;
int_to_type(6) -> grp_data;
int_to_type(7) -> anon_req;
int_to_type(8) -> path;
int_to_type(9) -> trace;
int_to_type(10) -> multipart;
int_to_type(11) -> control;
int_to_type(15) -> raw_custom;
int_to_type(N) -> {reserved, N}.

type_to_int(req) -> 0;
type_to_int(response) -> 1;
type_to_int(txt_msg) -> 2;
type_to_int(ack) -> 3;
type_to_int(advert) -> 4;
type_to_int(grp_txt) -> 5;
type_to_int(grp_data) -> 6;
type_to_int(anon_req) -> 7;
type_to_int(path) -> 8;
type_to_int(trace) -> 9;
type_to_int(multipart) -> 10;
type_to_int(control) -> 11;
type_to_int(raw_custom) -> 15;
type_to_int({reserved, N}) -> N.

node_type(1) -> chat;
node_type(2) -> repeater;
node_type(3) -> room_server;
node_type(4) -> sensor;
node_type(N) -> {unknown, N}.

node_type_to_int(chat) -> 1;
node_type_to_int(repeater) -> 2;
node_type_to_int(room_server) -> 3;
node_type_to_int(sensor) -> 4.

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.
