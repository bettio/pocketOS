-module(ed25519_x25519).

%%
%% X25519 ECDH shared secret from Ed25519 identity keys -- libsodium's
%% crypto_sign_ed25519_sk/pk_to_curve25519, reimplemented because AtomVM
%% does not expose it. AtomVM caps integers near 2^256, so the public-key
%% map u = (1 + y) / (1 - y) mod 2^255-19 avoids any 510-bit product:
%% double-and-add for the field multiply, and the extended Euclidean
%% algorithm for the modular inverse (far fewer big-int ops than Fermat's
%% a^(p-2), so much faster on AtomVM).
%%

-export([
    ed_secret_to_x25519/1,
    ed_pub_to_x25519/1,
    shared_secret/2
]).

%% curve25519 field prime.
-define(P, ((1 bsl 255) - 19)).

%% === private-key conversion ===

%% Ed25519 private key (32-byte seed or 64-byte seed||pub) to the X25519 scalar.
-spec ed_secret_to_x25519(binary()) -> binary().
ed_secret_to_x25519(<<Seed:32/binary>>) ->
    seed_to_x25519_scalar(Seed);
ed_secret_to_x25519(<<Seed:32/binary, _Pub:32/binary>>) ->
    seed_to_x25519_scalar(Seed);
ed_secret_to_x25519(Bin) when is_binary(Bin) ->
    error({invalid_private_key_size, byte_size(Bin)}).

seed_to_x25519_scalar(Seed) ->
    <<Lower:32/binary, _Upper:32/binary>> = crypto:hash(sha512, Seed),
    clamp(Lower).

clamp(<<First:8, Middle:30/binary, Last:8>>) ->
    <<(First band 248):8, Middle/binary, ((Last band 127) bor 64):8>>.

%% === public-key conversion ===

%% Compressed Ed25519 public key to the little-endian X25519 u-coordinate.
-spec ed_pub_to_x25519(<<_:256>>) -> <<_:256>>.
ed_pub_to_x25519(<<Pub:32/binary>>) ->
    P = ?P,
    <<Head:31/binary, Last:8>> = Pub,
    <<Y:256/little-unsigned-integer>> = <<Head/binary, (Last band 16#7F):8>>,
    (Y < P) orelse error({public_key_y_out_of_range, Y}),
    Numer = mod(1 + Y, P),
    Denom = mod(1 - Y, P),
    (Denom =/= 0) orelse error(public_key_denominator_zero),
    U = modmul(Numer, mod_inverse(Denom, P), P),
    int_to_le32(U);
ed_pub_to_x25519(Bin) when is_binary(Bin) ->
    error({invalid_public_key_size, byte_size(Bin)}).

%% === shared secret ===

%% Raw 32-byte X25519 shared secret from our Ed25519 key and a peer's Ed25519 pubkey.
-spec shared_secret(binary(), <<_:256>>) -> <<_:256>>.
shared_secret(MyEdSecret, PeerEdPub) ->
    MyScalar = ed_secret_to_x25519(MyEdSecret),
    PeerPublic = ed_pub_to_x25519(PeerEdPub),
    %% notsup/undef = no x25519 backend; any other failure or an all-zero
    %% result = a low-order/invalid peer key.
    Shared =
        try
            crypto:compute_key(eddh, PeerPublic, MyScalar, x25519)
        catch
            error:undef -> error(x25519_not_supported);
            error:notsup -> error(x25519_not_supported);
            error:{notsup, _, _} -> error(x25519_not_supported);
            error:_ -> error(low_order_public_key)
        end,
    case Shared of
        <<0:256>> -> error(low_order_public_key);
        <<_:32/binary>> -> Shared
    end.

%% === field / encoding helpers ===

mod(A, M) ->
    case A rem M of
        R when R < 0 -> R + M;
        R -> R
    end.

mod_inverse(A, M) ->
    ext_inv(M, A rem M, 0, 1, M).

ext_inv(R0, 0, S0, _S1, M) ->
    case R0 of
        1 -> ((S0 rem M) + M) rem M;
        _ -> error(no_inverse)
    end;
ext_inv(R0, R1, S0, S1, M) ->
    Q = R0 div R1,
    ext_inv(R1, R0 - Q * R1, S1, S0 - Q * S1, M).

modmul(A, B, M) ->
    modmul(mod(A, M), mod(B, M), M, 0).

modmul(_A, 0, _M, Acc) ->
    Acc;
modmul(A, B, M, Acc) ->
    Acc1 =
        case B band 1 of
            1 -> add_mod(Acc, A, M);
            0 -> Acc
        end,
    modmul(add_mod(A, A, M), B bsr 1, M, Acc1).

add_mod(X, Y, M) ->
    case X + Y of
        S when S >= M -> S - M;
        S -> S
    end.

int_to_le32(N) ->
    <<N:256/little-unsigned-integer>>.
