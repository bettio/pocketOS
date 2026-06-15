-module(meshtastic_server_core).
-moduledoc false.

%%
%% Functional core for `meshtastic_server`.
%%
%% This module holds the *thought concerns* of the Meshtastic node: dedup,
%% decrypt, decode, ACK / NodeInfo replies, flood-rebroadcast decisions and
%% packet-id generation. It is pure: it performs no I/O, reads no clock and
%% draws no randomness. Everything impure is injected by the gen_server
%% (`meshtastic_server`) as data through an `Env` map (`now_ms`, `rand22`, a
%% pre-resolved PKI `peer_key`), and everything the core wants done flows back
%% out as data: transmissions are appended to `tx_queue` in the returned state,
%% and other side effects are returned as an `Effects` list the gen_server runs.
%%

-include_lib("mesh_trace.hrl").

-export([
    init/2,
    handle_rx/4,
    handle_send/4,
    handle_periodic/2,
    handle_ack_timeout/2,
    can_send_pki/1,
    rx_needs_peer_key/2,
    rx_route_dests/2,
    next_packet_id/2,
    take_due/2,
    take_one_due/2,
    has_due/2,
    handle_tx_results/3,
    next_wakeup/2,
    rebroadcast_delay_ms/3,
    slot_time_ms/2,
    packet_airtime_ms/2,
    retransmission_ms/2,
    cw_size/1,
    update_last_seen/4,
    prune_expired_last_seen/2
]).

-export_type([core_state/0, effect/0, env/0, delivery_status/0]).

-record(core, {
    node_id :: non_neg_integer(),
    channel :: map(),
    private_key :: binary() | undefined,
    node_info :: map() | undefined,
    rolling_packet_id :: non_neg_integer(),
    last_seen = #{} :: map(),
    tx_queue = [] :: [tx_intent()],
    slot_time_ms = 28 :: pos_integer(),
    %% modulation parameters, for the airtime-derived retransmission interval
    spreading_factor = 11 :: pos_integer(),
    bandwidth_hz = 250000 :: pos_integer(),
    coding_rate = 5 :: 5..8,
    preamble_length = 16 :: pos_integer(),
    periodic_interval_ms = 60000 :: pos_integer(),
    default_hop_limit = 3 :: pos_integer(),
    node_info_want_response = true :: boolean(),
    ok_to_mqtt_bitfield = 1 :: non_neg_integer(),
    enable_relay = true :: boolean(),
    %% delivery-tracked sends awaiting an ack, keyed by packet_id
    pending = #{} :: map()
}).

-opaque core_state() :: #core{}.

-type effect() ::
    {deliver, DecodedPacket :: map()}
    | {learn, NodeId :: non_neg_integer(), NextHop :: 0..255}
    | {set_timer, Ms :: non_neg_integer(), Msg :: term()}
    | {notify, {pid(), reference()}, delivery_status()}.

%% Delivery-tracked send outcome; implicit = our packet overheard being
%% relayed, 'MAX_RETRANSMIT' = retransmissions exhausted.
-type delivery_status() ::
    {ack, #{implicit := boolean(), src => non_neg_integer()}}
    | {nak, atom()}.

%% - `now_ms` is the one injected clock: monotonic milliseconds, used for
%% rebroadcast / back-off / tx-scheduling deadlines.
%% - `rand22` is a fresh 22-bit value (top bits of an originated packet_id)
%% - `peer_key` is present only when rx_needs_peer_key/2 told the gen_server to resolve
%% it for a PKI packet addressed to us.
%% - `routes` maps dest node ids to learned next-hop bytes; `pki` selects PKI
%% crypto on a send.
%% - `notify` = {Pid, Ref} makes a send delivery-tracked.
-type env() :: #{
    now_ms => integer(),
    rand22 => non_neg_integer(),
    peer_key => {ok, binary()} | {error, term()},
    routes => map(),
    pki => boolean(),
    notify => {pid(), reference()}
}.

%% A queued transmission. `not_before` is `now` or an absolute monotonic-ms
%% deadline (SNR-weighted rebroadcast delay, or back-off after a failed send).
%% `attempts` sizes the back-off, never bounds retries; `ref` = {Src, PacketId}
%% tags relays and our tracked sends so a queued copy can be cancelled.
-type tx_intent() :: #{
    payload := binary(),
    not_before := now | integer(),
    attempts => non_neg_integer(),
    ref => {non_neg_integer(), non_neg_integer()}
}.

-define(PACKET_SEEN_EXPIRY_SEC, 30).
%% Mirror meshtastic packet-id generation: bottom 10 bits = rolling counter,
%% top 22 bits = fresh random per packet.
-define(PACKET_ID_COUNTER_MASK, 16#3FF).
-define(BROADCAST_ADDR, 16#FFFFFFFF).
-define(OK_TO_MQTT_BITFIELD, 1).
-define(DEFAULT_NODE_ID, 1127302788).
-define(INITIAL_PERIODIC_MS, 500).
-define(PERIODIC_MS, 60000).

%% want_ack sends: total transmissions before giving up, and the fixed
%% processing margin inside retransmission_ms/2.
-define(NUM_RELIABLE_RETX, 3).
-define(RETX_PROCESSING_TIME_MS, 4500).

%% Channel-access back-off (CSMA): re-enqueue a failed send at now + rand(0, window) ms,
%% window doubling per attempt up to 250 bsl 4 = 4000 ms. Unbounded.
-define(TX_BACKOFF_BASE_MS, 250).
-define(TX_BACKOFF_MAX_SHIFT, 4).

%% Defer each rebroadcast by `2*CWmax*slot + random(0, 2^CWsize)*slot`, where
%% CWsize maps our reception SNR (?SNR_MIN..?SNR_MAX dB) onto [?CW_MIN, ?CW_MAX],
%% so a weakly-heard (far) packet relays first -> range extension. We are role
%% CLIENT, hence the non-router branch (with the 2*CWmax*slot floor).
%%
%% The slot is a function of the modulation, so init/2 derives it from the
%% `spreading_factor` + `bandwidth_hz` opts via slot_time_ms/2, the same way
%% the meshtastic mainline firmware does:
%%   2.5 * 2^SF/BW + 7.6 ms
%% Examples (BW250):
%%   SF9  -> 2.5 * 2.048 + 7.6 ~= 13 ms
%%   SF11 -> 2.5 * 8.192 + 7.6 ~= 28 ms
%% ?DEFAULT_SLOT_TIME_MS is the fallback when the opts are absent/invalid; it
%% matches the live SF11/BW250 eu_433 long-fast preset.
-define(CW_MIN, 3).
-define(CW_MAX, 8).
-define(SNR_MIN, -20).
-define(SNR_MAX, 10).
-define(DEFAULT_SLOT_TIME_MS, 28).
%% Fallback modulation when the opts are absent/invalid (the live preset).
-define(DEFAULT_SPREADING_FACTOR, 11).
-define(DEFAULT_BANDWIDTH_HZ, 250000).
-define(DEFAULT_CODING_RATE, 5).
-define(DEFAULT_PREAMBLE_LENGTH, 16).

-define(ROUTE_SIZE, 8).
-define(NODENUM_BROADCAST, 16#FFFFFFFF).
-define(SNR_UNKNOWN, -128).

%%------------------------------------------------------------------------------
%% Construction
%%------------------------------------------------------------------------------

-spec init(proplists:proplist(), non_neg_integer()) -> {core_state(), [effect()]}.
init(MeshtasticOpts, InitialRolling) ->
    NodeId = proplists:get_value(node_id, MeshtasticOpts, ?DEFAULT_NODE_ID),
    NodeInfo = proplists:get_value(node_info, MeshtasticOpts),
    Channel = init_channel(proplists:get_value(channel, MeshtasticOpts)),
    PrivateKey = proplists:get_value(private_key, MeshtasticOpts),
    SfOpt = proplists:get_value(spreading_factor, MeshtasticOpts),
    BwOpt = proplists:get_value(bandwidth_hz, MeshtasticOpts),
    SlotTimeMs = slot_time_ms(SfOpt, BwOpt),
    {Sf, BwHz} = modulation(SfOpt, BwOpt),
    ?MESH_TRACE("[mesh] init node_id=~p initial_rolling=~p slot_time_ms=~p~n", [
        NodeId, InitialRolling, SlotTimeMs
    ]),
    Core = #core{
        node_id = NodeId,
        channel = Channel,
        private_key = PrivateKey,
        node_info = NodeInfo,
        rolling_packet_id = InitialRolling,
        slot_time_ms = SlotTimeMs,
        spreading_factor = Sf,
        bandwidth_hz = BwHz,
        coding_rate = coding_rate_denominator(proplists:get_value(coding_rate, MeshtasticOpts)),
        preamble_length = preamble_symbols(proplists:get_value(preamble_length, MeshtasticOpts)),
        periodic_interval_ms = proplists:get_value(periodic_interval_ms, MeshtasticOpts, ?PERIODIC_MS),
        default_hop_limit = proplists:get_value(default_hop_limit, MeshtasticOpts, 3),
        node_info_want_response = proplists:get_value(node_info_want_response, MeshtasticOpts, true),
        ok_to_mqtt_bitfield = proplists:get_value(
            ok_to_mqtt_bitfield, MeshtasticOpts, ?OK_TO_MQTT_BITFIELD
        ),
        enable_relay = proplists:get_value(enable_relay, MeshtasticOpts, true)
    },
    {Core, [{set_timer, ?INITIAL_PERIODIC_MS, periodic}]}.

init_channel(undefined) ->
    meshtastic:default_channel(<<"LongFast">>);
init_channel(#{name := Name, psk := Psk} = Channel) ->
    Channel#{hash => meshtastic:channel_hash(Name, Psk)}.

modulation(Sf, BwHz) when is_integer(Sf), is_integer(BwHz), BwHz > 0 ->
    {Sf, BwHz};
modulation(_Sf, _BwHz) ->
    {?DEFAULT_SPREADING_FACTOR, ?DEFAULT_BANDWIDTH_HZ}.

coding_rate_denominator(cr_4_5) -> 5;
coding_rate_denominator(cr_4_6) -> 6;
coding_rate_denominator(cr_4_7) -> 7;
coding_rate_denominator(cr_4_8) -> 8;
coding_rate_denominator(_) -> ?DEFAULT_CODING_RATE.

preamble_symbols(N) when is_integer(N), N > 0 -> N;
preamble_symbols(_) -> ?DEFAULT_PREAMBLE_LENGTH.

%%------------------------------------------------------------------------------
%% Receive
%%------------------------------------------------------------------------------

%% Handle a parsed inbound packet. The caller has already deserialized the wire
%% bytes (a parse failure never reaches here) and, for PKI packets to us,
%% pre-resolved `Env.peer_key`. Returns the reply atom the radio_manager
%% dispatch expects (`ok` | `discard`), the updated state (with `tx_queue`
%% already populated for any reply/rebroadcast), and the transient effects.
-spec handle_rx(map(), map(), env(), core_state()) -> {ok | discard, core_state(), [effect()]}.
handle_rx(#{src := Src, packet_id := PacketId}, _Attributes, _Env, #core{node_id = Src} = Core) ->
    case take_pending(PacketId, Core) of
        {undefined, _} ->
            ?MESH_TRACE("[mesh] rx discard (from this node)~n", []),
            {discard, Core, []};
        {Entry, Core1} ->
            %% Someone rebroadcast a tracked packet of ours: the implicit ack
            ?MESH_TRACE("[mesh] rx implicit ack pid=~p~n", [PacketId]),
            {discard, Core1, notify_effects(Entry, {ack, #{implicit => true}})}
    end;
handle_rx(#{src := Src, packet_id := PacketId} = Packet, Attributes, Env, Core) ->
    NowSec = maps:get(now_ms, Env) div 1000,
    {Duplicated, UpdatedLastSeen} = update_last_seen(Core#core.last_seen, Src, PacketId, NowSec),
    PrunedLastSeen = prune_expired_last_seen(UpdatedLastSeen, NowSec),
    IsRecipient = is_recipient(Packet, Core),
    if
        not Duplicated andalso IsRecipient ->
            handle_recipient(Packet, Attributes, Env, PrunedLastSeen, Core);
        not Duplicated ->
            ?MESH_TRACE("[mesh] rx not-for-us, will forward src=~p pid=~p~n", [Src, PacketId]),
            Core0 = Core#core{last_seen = PrunedLastSeen},
            Msg = relay_decode(Packet, Core0),
            Core1 = enqueue_rebroadcast(Packet, Msg, Attributes, Env, Core0),
            {ok, Core1, route_learn_effects(Packet, Msg, Core0)};
        true ->
            ?MESH_TRACE("[mesh] rx duplicate src=~p pid=~p~n", [Src, PacketId]),
            {discard, dupe_re_ack(Packet, Env, Core), []}
    end.

%% Recipient packet: decrypt, decode, deliver, then maybe reply and rebroadcast.
handle_recipient(Packet, Attributes, Env, PrunedLastSeen, Core) ->
    case try_decrypt(Packet, Env, Core) of
        {ok, #{data := Decrypted} = DecryptedPacket} ->
            try meshtastic_proto:decode(Decrypted) of
                Message ->
                    WantResponse = maps:get(want_response, Message, false),
                    DecodedPacket = DecryptedPacket#{
                        message => Message,
                        want_response => WantResponse,
                        rssi => maps:get(rssi, Attributes, undefined),
                        snr => maps:get(snr, Attributes, undefined)
                    },
                    Core1 = Core#core{last_seen = PrunedLastSeen},
                    {Core1b, AckEffects} = match_pending(Packet, Message, Core1),
                    {Core2, ReplyEffects} =
                        enqueue_replies(Packet, Message, Attributes, Env, Core1b),
                    Core3 = enqueue_rebroadcast(Packet, Message, Attributes, Env, Core2),
                    LearnEffects = route_learn_effects(Packet, Message, Core1b),
                    {ok, Core3,
                        AckEffects ++ [{deliver, DecodedPacket} | LearnEffects] ++ ReplyEffects}
            catch
                _:_ ->
                    ?MESH_TRACE("[mesh] rx discard (protobuf decode failed): ~p~n", [Decrypted]),
                    {discard, Core, []}
            end;
        {error, _Reason} ->
            ?MESH_TRACE("[mesh] rx discard (decrypt failed): ~p~n", [_Reason]),
            {discard, Core, []}
    end.

%% PKI mode is signalled by channel_hash == 0 on a unicast packet to us; the
%% peer's public key was pre-resolved by the caller into Env.peer_key. Everything
%% else decrypts with the channel PSK.
try_decrypt(
    #{channel_hash := 0, dest := Dest} = Packet,
    Env,
    #core{node_id = Dest, private_key = OurPriv}
) when OurPriv =/= undefined, Dest =/= ?BROADCAST_ADDR ->
    case maps:get(peer_key, Env, {error, missing_peer_key}) of
        {ok, PeerPub} -> meshtastic:decrypt_pki(Packet, OurPriv, PeerPub);
        {error, _} = Error -> Error
    end;
try_decrypt(Packet, _Env, #core{channel = #{psk := Psk}}) ->
    {ok, meshtastic:decrypt(Packet, Psk)}.

%% Pure predicate: does this rx require the gen_server to pre-resolve a peer public
%% key? Mirrors the PKI clause guard of try_decrypt/3 exactly.
-spec rx_needs_peer_key(map(), core_state()) -> boolean().
rx_needs_peer_key(
    #{channel_hash := 0, dest := Dest},
    #core{node_id = Dest, private_key = Priv}
) when Priv =/= undefined, Dest =/= ?BROADCAST_ADDR ->
    true;
rx_needs_peer_key(_Packet, _Core) ->
    false.

is_recipient(#{dest := Dest}, #core{node_id = NodeId}) ->
    case Dest of
        NodeId -> true;
        ?BROADCAST_ADDR -> true;
        _ -> false
    end.

-spec rx_route_dests(map(), core_state()) -> [non_neg_integer()].
rx_route_dests(
    #{dest := Dest, src := Src, next_hop := NextHop, hop_limit := HopLimit} = Packet,
    #core{node_id = NodeId} = Core
) ->
    ReplyDests =
        case is_recipient(Packet, Core) of
            true -> [Src];
            false -> []
        end,
    RelayDests =
        case
            Dest =/= NodeId andalso HopLimit > 0 andalso
                NextHop =:= meshtastic:relay_node_byte(NodeId)
        of
            true -> [Dest];
            false -> []
        end,
    ReplyDests ++ RelayDests.

%% At most one reply is produced, in precedence order: a TRACEROUTE route_reply,
%% then a NodeInfo want_response reply, else a ROUTING ACK. The first two carry
%% `request_id` and so double as the implicit ACK, replacing the bare ACK.
%% Returns {Core, Effects}; a tracked reply contributes its ack-timeout timer.
enqueue_replies(Packet, Message, Attributes, Env, Core) ->
    case traceroute_reply(Packet, Message, Attributes, Env, Core) of
        {true, Core1} ->
            {Core1, []};
        {false, Core1} ->
            case node_info_reply(Packet, Message, Env, Core1) of
                {true, Core2} -> {Core2, []};
                {false, Core2} -> ack_reply(Packet, Message, Env, Core2)
            end
    end.

traceroute_reply(
    #{src := Src, dest := Dest, packet_id := OrigPid} = Packet,
    #{portnum := 'TRACEROUTE_APP', want_response := true} = Message,
    Attributes,
    Env,
    #core{node_id = Dest} = Core
) when Dest =/= ?BROADCAST_ADDR ->
    Route0 = maps:get(payload, Message, #{}),
    SnrByte = snr_byte(maps:get(snr, Attributes, undefined)),
    Route1 = annotate_route(Route0, true, true, Dest, SnrByte, hops_taken(Packet)),
    Data = #{
        portnum => 'TRACEROUTE_APP',
        payload => Route1,
        request_id => OrigPid,
        bitfield => Core#core.ok_to_mqtt_bitfield
    },
    Bin = erlang:iolist_to_binary(meshtastic_proto:encode(Data)),
    ?MESH_TRACE("[mesh] traceroute reply -> src=~p req=~p snr=~p~n", [Src, OrigPid, SnrByte]),
    {true, originate(Src, Bin, Env, Core)};
traceroute_reply(_Packet, _Message, _Attributes, _Env, Core) ->
    {false, Core}.

snr_byte(undefined) -> ?SNR_UNKNOWN;
snr_byte(Snr) -> clamp(trunc(Snr * 4), -127, 127).

annotate_route(Route, IsTowardsDest, SnrOnly, NodeId, SnrByte, HopsTaken) ->
    {RouteKey, SnrKey} =
        case IsTowardsDest of
            true -> {route, snr_towards};
            false -> {route_back, snr_back}
        end,
    R0 = maps:get(RouteKey, Route, []),
    S0 = maps:get(SnrKey, Route, []),
    R1 = pad_to(R0, HopsTaken, ?NODENUM_BROADCAST),
    S1 = pad_to(S0, length(R1), ?SNR_UNKNOWN),
    S2 = append_capped(S1, SnrByte),
    R2 =
        case SnrOnly of
            true -> R1;
            false -> append_capped(R1, NodeId)
        end,
    Route#{RouteKey => R2, SnrKey => S2}.

pad_to(List, Target, Fill) ->
    Want = min(max(Target, length(List)), ?ROUTE_SIZE),
    List ++ lists:duplicate(Want - length(List), Fill).

append_capped(List, X) ->
    case length(List) < ?ROUTE_SIZE of
        true -> List ++ [X];
        false -> List
    end.

hops_taken(#{hop_start := HopStart, hop_limit := HopLimit}) when HopStart > 0, HopLimit =< HopStart ->
    HopStart - HopLimit;
hops_taken(_) ->
    0.

node_info_reply(
    #{src := Src, packet_id := OrigPid},
    #{portnum := 'NODEINFO_APP', want_response := true},
    Env,
    #core{node_info = #{user_info := _UserInfo}} = Core
) ->
    ?MESH_TRACE("[mesh] node_info reply -> src=~p req=~p~n", [Src, OrigPid]),
    {true, send_node_info(Src, #{request_id => OrigPid}, Env, Core)};
node_info_reply(_Packet, _Message, _Env, Core) ->
    {false, Core}.

ack_reply(
    #{src := Src, dest := Dest, want_ack := true, packet_id := OrigPid} = Packet,
    Message,
    Env,
    #core{node_id = Dest} = Core
) when Dest =/= ?BROADCAST_ADDR ->
    case maps:is_key(request_id, Message) of
        true ->
            %% An ack/reply that itself wants an ack: never ack it in full
            ack_the_ack(Packet, Env, Core);
        false ->
            AckBin = ack_data(OrigPid, Core#core.ok_to_mqtt_bitfield),
            case Message of
                #{portnum := 'TEXT_MESSAGE_APP'} ->
                    %% meshtastic acks text DMs reliably (tracked want_ack send)
                    ?MESH_TRACE("[mesh] reliable ack pid=~p -> src=~p~n", [OrigPid, Src]),
                    originate_tracked(Src, AckBin, undefined, Env, Core);
                _ ->
                    ?MESH_TRACE("[mesh] ack pid=~p -> src=~p~n", [OrigPid, Src]),
                    {originate(Src, AckBin, Env, Core), []}
            end
    end;
ack_reply(_Packet, _Message, _Env, Core) ->
    {Core, []}.

%% A directly-heard or next-hop-addressed want_ack ack gets a 0-hop ack back,
%% which terminates the ack chain.
ack_the_ack(
    #{src := Src, packet_id := OrigPid, next_hop := NextHop} = Packet,
    Env,
    #core{node_id = NodeId} = Core
) ->
    case hops_taken(Packet) =:= 0 orelse NextHop =:= meshtastic:relay_node_byte(NodeId) of
        true ->
            ?MESH_TRACE("[mesh] 0-hop ack-the-ack pid=~p -> src=~p~n", [OrigPid, Src]),
            {originate(Src, ack_data(OrigPid, Core#core.ok_to_mqtt_bitfield), #{hop => 0}, Env, Core), []};
        false ->
            {Core, []}
    end.

ack_data(RequestId, Bitfield) ->
    AckData = #{
        portnum => 'ROUTING_APP',
        payload => #{error_reason => 'NONE'},
        request_id => RequestId,
        bitfield => Bitfield
    },
    erlang:iolist_to_binary(meshtastic_proto:encode(AckData)).

%% Rebroadcast the original (still-encrypted) packet with hop_limit decremented,
%% relay_node stamped to our last byte, and next_hop re-derived: a flood stays a
%% flood, while a packet that named us as the next hop is re-directed toward its
%% dest from our learned routes (else falls back to flooding). Skipped for
%% unicasts to us, exhausted hop limits, and packets directed at another named hop.
enqueue_rebroadcast(_Packet, _MaybeMessage, _Attributes, _Env, #core{enable_relay = false} = Core) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (relay disabled)~n", []),
    Core;
enqueue_rebroadcast(
    #{dest := Dest} = _Packet, _MaybeMessage, _Attributes, _Env, #core{node_id = Dest} = Core
) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (unicast-to-us)~n", []),
    Core;
enqueue_rebroadcast(#{hop_limit := 0} = _Packet, _MaybeMessage, _Attributes, _Env, Core) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (hop=0)~n", []),
    Core;
enqueue_rebroadcast(
    #{
        dest := Dest,
        hop_limit := HopLimit,
        next_hop := NextHop,
        src := Src,
        packet_id := PacketId
    } = Packet,
    MaybeMessage,
    Attributes,
    Env,
    #core{node_id = NodeId} = Core
) ->
    RelayByte = meshtastic:relay_node_byte(NodeId),
    if
        NextHop =/= 0 andalso NextHop =/= RelayByte ->
            ?MESH_TRACE("[mesh] rebroadcast skip (next_hop=~p not us)~n", [NextHop]),
            Core;
        true ->
            NewNextHop =
                case NextHop of
                    0 -> 0;
                    _ -> next_hop_for(Dest, RelayByte, maps:get(routes, Env, #{}))
                end,
            EncData = relay_encrypted_data(Packet, MaybeMessage, Attributes, Core),
            RadioPayload = meshtastic:serialize(Packet#{
                hop_limit := HopLimit - 1,
                next_hop := NewNextHop,
                relay_node := RelayByte,
                encrypted_data := EncData
            }),
            Snr = maps:get(snr, Attributes, undefined),
            Delay = rebroadcast_delay_ms(Snr, maps:get(rand22, Env), Core#core.slot_time_ms),
            NotBefore = maps:get(now_ms, Env) + Delay,
            ?MESH_TRACE(
                "[mesh] rebroadcast pid=~p hop=~p->~p next_hop=~p relay=~p snr=~p in ~pms~n",
                [PacketId, HopLimit, HopLimit - 1, NewNextHop, RelayByte, Snr, Delay]
            ),
            enqueue_intent(
                #{payload => RadioPayload, not_before => NotBefore, ref => {Src, PacketId}},
                Core
            )
    end.

relay_encrypted_data(
    #{channel_hash := Hash} = Packet,
    #{portnum := 'TRACEROUTE_APP'} = Message,
    Attributes,
    #core{channel = #{hash := Hash, psk := Psk}, node_id = NodeId}
) ->
    Route0 = maps:get(payload, Message, #{}),
    IsTowardsDest = maps:get(request_id, Message, 0) =:= 0,
    SnrByte = snr_byte(maps:get(snr, Attributes, undefined)),
    Route1 = annotate_route(Route0, IsTowardsDest, false, NodeId, SnrByte, hops_taken(Packet)),
    NewData = erlang:iolist_to_binary(meshtastic_proto:encode(Message#{payload => Route1})),
    Enc = meshtastic:encrypt(Packet#{data => NewData}, Psk),
    maps:get(encrypted_data, Enc);
relay_encrypted_data(Packet, _MaybeMessage, _Attributes, _Core) ->
    maps:get(encrypted_data, Packet).

relay_decode(#{channel_hash := Hash} = Packet, #core{channel = #{hash := Hash, psk := Psk}}) ->
    try
        #{data := Data} = meshtastic:decrypt(Packet, Psk),
        meshtastic_proto:decode(Data)
    catch
        _:_ -> undefined
    end;
relay_decode(_Packet, _Core) ->
    undefined.

route_learn_effects(_Packet, undefined, _Core) ->
    [];
route_learn_effects(#{src := Src, relay_node := Relay} = Packet, Message, Core) ->
    IsAckOrReply =
        maps:get(request_id, Message, 0) =/= 0 orelse maps:get(reply_id, Message, 0) =/= 0,
    RelayLearn =
        case IsAckOrReply andalso Relay =/= 0 of
            true -> [{learn, Src, Relay}];
            false -> []
        end,
    RelayLearn ++ traceroute_route_learn(Packet, Message, Core).

traceroute_route_learn(
    #{src := ReplySrc, dest := Dest, channel_hash := Hash},
    #{portnum := 'TRACEROUTE_APP', request_id := ReqId} = Message,
    #core{channel = #{hash := Hash}, node_id = NodeId}
) when ReqId =/= 0 ->
    Route = maps:get(route, maps:get(payload, Message, #{}), []),
    NextHopIndex =
        if
            Dest =:= NodeId -> 0;
            true -> idx_after(NodeId, Route)
        end,
    case NextHopIndex of
        none ->
            [];
        _ ->
            Tail = lists:nthtail(NextHopIndex, Route),
            NextHop =
                case Tail of
                    [] -> ReplySrc;
                    [Next | _] -> Next
                end,
            case NextHop of
                ?NODENUM_BROADCAST ->
                    [];
                _ ->
                    Byte = meshtastic:relay_node_byte(NextHop),
                    Downstream = [N || N <- Tail, N =/= ?NODENUM_BROADCAST],
                    [{learn, Target, Byte} || Target <- Downstream ++ [ReplySrc]]
            end
    end;
traceroute_route_learn(_Packet, _Message, _Core) ->
    [].

idx_after(X, List) -> idx_after(X, List, 0).
idx_after(_X, [], _I) -> none;
idx_after(X, [X | _], I) -> I + 1;
idx_after(X, [_ | T], I) -> idx_after(X, T, I + 1).

%%------------------------------------------------------------------------------
%% Delivery tracking (want_ack sends awaiting an ack / nak / give-up)
%%------------------------------------------------------------------------------

%% A packet strictly to us whose request_id names a pending send resolves it:
%% a ROUTING error is a nak, anything else confirms delivery.
match_pending(
    #{dest := Dest, src := Src},
    Message,
    #core{node_id = Dest} = Core
) when Dest =/= ?BROADCAST_ADDR ->
    case maps:get(request_id, Message, 0) of
        0 ->
            {Core, []};
        ReqId ->
            case take_pending(ReqId, Core) of
                {undefined, _} ->
                    {Core, []};
                {Entry, Core1} ->
                    Status = ack_or_nak(Message, Src),
                    ?MESH_TRACE("[mesh] rx resolves pid=~p: ~p~n", [ReqId, Status]),
                    {Core1, notify_effects(Entry, Status)}
            end
    end;
match_pending(_Packet, _Message, Core) ->
    {Core, []}.

ack_or_nak(#{portnum := 'ROUTING_APP'} = Message, Src) ->
    case maps:get(error_reason, maps:get(payload, Message, #{}), 'NONE') of
        'NONE' -> {ack, #{implicit => false, src => Src}};
        Reason -> {nak, Reason}
    end;
ack_or_nak(_Message, Src) ->
    {ack, #{implicit => false, src => Src}}.

%% A directly-heard duplicate of a want_ack packet to us means our ack was
%% lost: ack it again, 0-hop.
dupe_re_ack(
    #{dest := Dest, want_ack := true, packet_id := OrigPid, src := Src} = Packet,
    Env,
    #core{node_id = Dest} = Core
) when Dest =/= ?BROADCAST_ADDR ->
    case hops_taken(Packet) of
        0 ->
            ?MESH_TRACE("[mesh] re-ack dupe pid=~p -> src=~p~n", [OrigPid, Src]),
            originate(Src, ack_data(OrigPid, Core#core.ok_to_mqtt_bitfield), #{hop => 0}, Env, Core);
        _ ->
            Core
    end;
dupe_re_ack(_Packet, _Env, Core) ->
    Core.

%% Remove a pending send, cancelling any still-queued intent for it.
take_pending(PacketId, #core{pending = Pending, node_id = NodeId} = Core) ->
    case maps:find(PacketId, Pending) of
        {ok, Entry} ->
            Core1 = Core#core{pending = maps:remove(PacketId, Pending)},
            {Entry, cancel_intents({NodeId, PacketId}, Core1)};
        error ->
            {undefined, Core}
    end.

notify_effects(#{notify := {Pid, Ref}}, Status) -> [{notify, {Pid, Ref}, Status}];
notify_effects(_Entry, _Status) -> [].

%% Drop queued intents tagged with Ref.
cancel_intents(Ref, #core{tx_queue = Queue} = Core) ->
    Core#core{tx_queue = [I || I <- Queue, maps:get(ref, I, undefined) =/= Ref]}.

%% Retransmit a pending send and re-arm, or give up with a MAX_RETRANSMIT nak;
%% a missing entry is a stale timer (already resolved).
-spec handle_ack_timeout(non_neg_integer(), core_state()) -> {core_state(), [effect()]}.
handle_ack_timeout(PacketId, #core{pending = Pending, node_id = NodeId} = Core) ->
    case maps:find(PacketId, Pending) of
        {ok, #{tries_left := 0} = Entry} ->
            ?MESH_TRACE("[mesh] reliable send failed pid=~p~n", [PacketId]),
            {_, Core1} = take_pending(PacketId, Core),
            {Core1, notify_effects(Entry, {nak, 'MAX_RETRANSMIT'})};
        {ok, #{tries_left := Tries, interval_ms := Interval, packet := Packet, dest := Dest} = Entry} ->
            %% Last unicast retry: fall back to flooding
            Packet1 =
                case Tries =:= 1 andalso Dest =/= ?BROADCAST_ADDR of
                    true -> Packet#{next_hop := 0};
                    false -> Packet
                end,
            ?MESH_TRACE("[mesh] retransmit pid=~p tries_left=~p~n", [PacketId, Tries - 1]),
            Entry1 = Entry#{tries_left := Tries - 1, packet := Packet1},
            Core1 = Core#core{pending = maps:put(PacketId, Entry1, Pending)},
            Core2 = enqueue_intent(
                #{
                    payload => meshtastic:serialize(Packet1),
                    not_before => now,
                    ref => {NodeId, PacketId}
                },
                Core1
            ),
            {Core2, [{set_timer, Interval, {ack_timeout, PacketId}}]};
        error ->
            {Core, []}
    end.

%%------------------------------------------------------------------------------
%% Send / periodic
%%------------------------------------------------------------------------------

-spec handle_send(non_neg_integer(), binary(), env(), core_state()) ->
    {ok, core_state(), [effect()]}.
handle_send(Dest, Data, Env, Core) ->
    case maps:get(notify, Env, undefined) of
        undefined ->
            {ok, originate(Dest, Data, Env, Core), []};
        Notify ->
            {Core1, Effects} = originate_tracked(Dest, Data, Notify, Env, Core),
            {ok, Core1, Effects}
    end.

-spec can_send_pki(core_state()) -> boolean().
can_send_pki(#core{private_key = Priv}) -> Priv =/= undefined.

-spec handle_periodic(env(), core_state()) -> {core_state(), [effect()]}.
handle_periodic(Env, #core{node_info = #{user_info := _UserInfo}} = Core) ->
    ?MESH_TRACE("[mesh] periodic node_info broadcast~n", []),
    Extra =
        case Core#core.node_info_want_response of
            true -> #{want_response => true};
            false -> #{}
        end,
    Core1 = send_node_info(?BROADCAST_ADDR, Extra, Env, Core),
    {Core1, [{set_timer, Core#core.periodic_interval_ms, periodic}]};
handle_periodic(_Env, Core) ->
    ?MESH_TRACE("[mesh] periodic skip (no user_info)~n", []),
    {Core, []}.

send_node_info(Dest, Extra, Env, #core{node_info = #{user_info := UserInfo}} = Core) ->
    Data = maps:merge(
        #{portnum => 'NODEINFO_APP', payload => UserInfo, bitfield => Core#core.ok_to_mqtt_bitfield},
        Extra
    ),
    Bin = erlang:iolist_to_binary(meshtastic_proto:encode(Data)),
    originate(Dest, Bin, Env, Core);
send_node_info(_Dest, _Extra, _Env, Core) ->
    ?MESH_TRACE("[mesh] node_info send skip (no user_info)~n", []),
    Core.

-spec next_hop_for(non_neg_integer(), 0..255, map()) -> 0..255.
next_hop_for(?BROADCAST_ADDR, _OurByte, _Routes) ->
    0;
next_hop_for(Dest, OurByte, Routes) ->
    case maps:get(Dest, Routes, 0) of
        0 -> 0;
        OurByte -> 0;
        Byte -> Byte
    end.

%% Build a freshly-originated packet (new packet_id from the rolling counter +
%% Env.rand22), encrypt it (channel PSK, or PKI when Env asks for it), serialize
%% and enqueue for TX. Opts: `want_ack` (default false), `hop` (default 3).
originate(Dest, Data, Env, Core) ->
    originate(Dest, Data, #{}, Env, Core).

originate(Dest, Data, Opts, Env, Core) ->
    {_PacketId, _Encrypted, RadioPayload, Core1} = build_packet(Dest, Data, Opts, Env, Core),
    enqueue_tx(RadioPayload, now, Core1).

%% Originate a delivery-tracked (want_ack) send: pending entry + first TX +
%% ack-timeout timer. Notify is {Pid, Ref}, or undefined to resolve silently.
originate_tracked(Dest, Data, Notify, Env, Core) ->
    {PacketId, Encrypted, RadioPayload, Core1} =
        build_packet(Dest, Data, #{want_ack => true}, Env, Core),
    Interval = retransmission_ms(byte_size(RadioPayload), Core1),
    Entry = #{
        packet => Encrypted,
        dest => Dest,
        notify => Notify,
        tries_left => ?NUM_RELIABLE_RETX - 1,
        interval_ms => Interval
    },
    Core2 = Core1#core{pending = maps:put(PacketId, Entry, Core1#core.pending)},
    Core3 = enqueue_intent(
        #{
            payload => RadioPayload,
            not_before => now,
            ref => {Core1#core.node_id, PacketId}
        },
        Core2
    ),
    {Core3, [{set_timer, Interval, {ack_timeout, PacketId}}]}.

build_packet(Dest, Data, Opts, Env, #core{
    node_id = NodeId,
    rolling_packet_id = Rolling
} = Core) ->
    Rand22 = maps:get(rand22, Env),
    {PacketId, NextRolling} = next_packet_id(Rolling, Rand22),
    RelayByte = meshtastic:relay_node_byte(NodeId),
    {ChannelHash, Encrypt} = tx_crypto(Dest, Env, Core),
    Hop = maps:get(hop, Opts, Core#core.default_hop_limit),
    Packet = #{
        dest => Dest,
        src => NodeId,
        packet_id => PacketId,
        hop_start => Hop,
        via_mqtt => false,
        want_ack => maps:get(want_ack, Opts, false),
        hop_limit => Hop,
        channel_hash => ChannelHash,
        next_hop => next_hop_for(Dest, RelayByte, maps:get(routes, Env, #{})),
        relay_node => RelayByte,
        data => Data
    },
    Encrypted = Encrypt(Packet),
    RadioPayload = meshtastic:serialize(Encrypted),
    ?MESH_TRACE(
        "[mesh] tx pid=~p dest=~p data_bytes=~p wire_bytes=~p~n",
        [PacketId, Dest, byte_size(Data), byte_size(RadioPayload)]
    ),
    {PacketId, Encrypted, RadioPayload, Core#core{rolling_packet_id = NextRolling}}.

tx_crypto(Dest, #{pki := true, peer_key := {ok, Pub}}, #core{private_key = Priv}) when
    Priv =/= undefined, Dest =/= ?BROADCAST_ADDR
->
    {0, fun(Packet) -> meshtastic:encrypt_pki(Packet, Priv, Pub) end};
tx_crypto(_Dest, _Env, #core{channel = #{psk := Psk, hash := Hash}}) ->
    {Hash, fun(Packet) -> meshtastic:encrypt(Packet, Psk) end}.

-spec next_packet_id(non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
next_packet_id(Rolling, Rand22) ->
    NextRolling = (Rolling + 1) band ?PACKET_ID_COUNTER_MASK,
    PacketId = NextRolling bor (Rand22 bsl 10),
    {PacketId, NextRolling}.

%%------------------------------------------------------------------------------
%% TX queue ADT (a plain FIFO list behind these accessors)
%%------------------------------------------------------------------------------

enqueue_tx(Payload, NotBefore, Core) ->
    enqueue_intent(#{payload => Payload, not_before => NotBefore}, Core).

enqueue_intent(Intent, #core{tx_queue = Queue} = Core) ->
    Core#core{tx_queue = Queue ++ [Intent]}.

%% Pop every due intent (FIFO), removing it from the queue. The caller sends each
%% payload and feeds the result back via handle_tx_results/3.
-spec take_due(core_state(), integer()) -> {[tx_intent()], core_state()}.
take_due(#core{tx_queue = Queue} = Core, Now) ->
    {Due, Pending} = lists_partition(fun(Intent) -> is_due(Intent, Now) end, Queue),
    {Due, Core#core{tx_queue = Pending}}.

is_due(#{not_before := now}, _Now) -> true;
is_due(#{not_before := NotBefore}, Now) -> NotBefore =< Now.

-spec take_one_due(core_state(), integer()) ->
    {tx_intent(), core_state()} | {none, core_state()}.
take_one_due(#core{tx_queue = Queue} = Core, Now) ->
    case take_one_due(Queue, Now, []) of
        none -> {none, Core};
        {Intent, Rest} -> {Intent, Core#core{tx_queue = Rest}}
    end.

take_one_due([], _Now, _Skipped) ->
    none;
take_one_due([Intent | Tail], Now, Skipped) ->
    case is_due(Intent, Now) of
        true -> {Intent, lists:reverse(Skipped) ++ Tail};
        false -> take_one_due(Tail, Now, [Intent | Skipped])
    end.

-spec has_due(core_state(), integer()) -> boolean().
has_due(#core{tx_queue = Queue}, Now) ->
    has_due_list(Queue, Now).

has_due_list([], _Now) -> false;
has_due_list([Intent | Tail], Now) ->
    case is_due(Intent, Now) of
        true -> true;
        false -> has_due_list(Tail, Now)
    end.

%% Delay (ms) until the earliest not-yet-due intent, or `infinity` if none.
%% Call after take_due/2 has drained the due intents.
-spec next_wakeup(core_state(), integer()) -> non_neg_integer() | infinity.
next_wakeup(#core{tx_queue = Queue}, Now) ->
    case [NotBefore || #{not_before := NotBefore} <- Queue, is_integer(NotBefore)] of
        [] -> infinity;
        Deadlines -> max(0, lists_min(Deadlines) - Now)
    end.

%% Feed each pumped intent's result back: `ok` is a no-op (take_due dropped it),
%% `{error, payload_too_large}` is a permanent drop, any other error is transient
%% (re-enqueue with back-off, unbounded retry). The batch shares one `rand22`,
%% decorrelated per intent by index+attempts so co-failing intents don't re-collide.
-spec handle_tx_results([{tx_intent(), ok | {error, term()}}], env(), core_state()) ->
    core_state().
handle_tx_results(Results, Env, Core) ->
    handle_tx_results(Results, Env, Core, 0).

handle_tx_results([], _Env, Core, _Index) ->
    Core;
handle_tx_results([{_Intent, ok} | Rest], Env, Core, Index) ->
    handle_tx_results(Rest, Env, Core, Index + 1);
handle_tx_results([{_Intent, {error, payload_too_large}} | Rest], Env, Core, Index) ->
    ?MESH_TRACE("[mesh] tx drop (payload_too_large) bytes=~p~n", [
        byte_size(maps:get(payload, _Intent))
    ]),
    handle_tx_results(Rest, Env, Core, Index + 1);
handle_tx_results(
    [{Intent, {error, _Reason}} | Rest], Env, #core{tx_queue = Queue} = Core, Index
) ->
    #{now_ms := Now, rand22 := Rand} = Env,
    Attempts = maps:get(attempts, Intent, 0),
    Seed = Rand bxor (Index bsl 16) bxor (Attempts bsl 8),
    Backoff = backoff_ms(Attempts, Seed),
    ?MESH_TRACE("[mesh] tx retry (~p) attempt=~p in ~pms~n", [_Reason, Attempts + 1, Backoff]),
    Intent1 = Intent#{not_before => Now + Backoff, attempts => Attempts + 1},
    handle_tx_results(Rest, Env, Core#core{tx_queue = Queue ++ [Intent1]}, Index + 1).

-spec backoff_ms(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
backoff_ms(Attempts, Rand) ->
    Window = ?TX_BACKOFF_BASE_MS bsl min(Attempts, ?TX_BACKOFF_MAX_SHIFT),
    Rand rem Window.

-spec rebroadcast_delay_ms(integer() | undefined, non_neg_integer(), pos_integer()) ->
    non_neg_integer().
rebroadcast_delay_ms(Snr, Rand, SlotMs) ->
    CWsize = cw_size(Snr),
    Offset = 2 * ?CW_MAX * SlotMs,
    Window = 1 bsl CWsize,
    Offset + (Rand rem Window) * SlotMs.

-spec slot_time_ms(term(), term()) -> pos_integer().
slot_time_ms(Sf, BwHz) when is_integer(Sf), is_integer(BwHz), BwHz > 0 ->
    round(2.5 * (1 bsl Sf) * 1000 / BwHz + 7.6);
slot_time_ms(_Sf, _BwHz) ->
    ?DEFAULT_SLOT_TIME_MS.

%% LoRa time-on-air (ms) of a TotalBytes-long wire frame under the configured
%% modulation (the standard formula, as meshtastic computes it).
-spec packet_airtime_ms(pos_integer(), core_state()) -> non_neg_integer().
packet_airtime_ms(TotalBytes, #core{
    spreading_factor = Sf,
    bandwidth_hz = BwHz,
    coding_rate = Cr,
    preamble_length = PreambleLen
}) ->
    TSymMs = (1 bsl Sf) * 1000 / BwHz,
    LowDataOpt =
        case TSymMs > 16.0 of
            true -> 1;
            false -> 0
        end,
    TPreambleMs = (PreambleLen + 4.25) * TSymMs,
    Num = 8 * TotalBytes - 4 * Sf + 28 + 16,
    Den = 4 * (Sf - 2 * LowDataOpt),
    NPayload = 8 + max(ceil(Num / Den) * Cr, 0),
    trunc(TPreambleMs + NPayload * TSymMs).

%% Wait before retransmitting a tracked send; the meshtastic interval with the
%% contention term pinned at CWmin (utilization untracked). The airtime input
%% counts the header twice, as meshtastic does; kept for equal timing.
-spec retransmission_ms(pos_integer(), core_state()) -> pos_integer().
retransmission_ms(WireBytes, #core{slot_time_ms = SlotMs} = Core) ->
    Airtime = packet_airtime_ms(WireBytes + 16, Core),
    Slots = (1 bsl ?CW_MIN) + 2 * ?CW_MAX + (1 bsl ((?CW_MAX + ?CW_MIN) div 2)),
    2 * Airtime + Slots * SlotMs + ?RETX_PROCESSING_TIME_MS.

-spec cw_size(integer() | undefined) -> non_neg_integer().
cw_size(Snr) when is_integer(Snr) ->
    clamp(?CW_MIN + (Snr - ?SNR_MIN) * (?CW_MAX - ?CW_MIN) div (?SNR_MAX - ?SNR_MIN), ?CW_MIN, ?CW_MAX);
cw_size(_) ->
    ?CW_MAX.

clamp(V, Lo, _Hi) when V < Lo -> Lo;
clamp(V, _Lo, Hi) when V > Hi -> Hi;
clamp(V, _Lo, _Hi) -> V.

%%------------------------------------------------------------------------------
%% Dedup
%%------------------------------------------------------------------------------

%% returns: {AlreadySeen, UpdatedLastSeenMap}
update_last_seen(LastSeenMap, Source, PacketId, MonotonicSec) ->
    case LastSeenMap of
        #{Source := #{PacketId := LastTimestamp} = SeenPacketMap} ->
            if
                MonotonicSec > LastTimestamp ->
                    {true, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
                true ->
                    {true, LastSeenMap}
            end;
        #{Source := SeenPacketMap} ->
            {false, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
        _ ->
            {false, LastSeenMap#{Source => #{PacketId => MonotonicSec}}}
    end.

prune_expired_last_seen(LastSeenMap, MonotonicSec) ->
    PrunedMap =
        maps:map(
            fun(_Source, SeenPacketMap) ->
                maps:filter(
                    fun(_PacketId, LastTimestamp) ->
                        if
                            LastTimestamp + ?PACKET_SEEN_EXPIRY_SEC < MonotonicSec ->
                                false;
                            true ->
                                true
                        end
                    end,
                    SeenPacketMap
                )
            end,
            LastSeenMap
        ),
    maps:filter(fun(_Source, SeenPacketMap) -> SeenPacketMap =/= #{} end, PrunedMap).

%%------------------------------------------------------------------------------
%% Local list helpers
%%
%% TODO: drop these and call lists:partition/2 and lists:min/1 directly once
%% AtomVM exposes them.
%% Ported verbatim from OTP lib/stdlib/src/lists.erl.
%%------------------------------------------------------------------------------

lists_partition(Pred, List) when is_function(Pred, 1) ->
    lists_partition(Pred, List, [], []).

lists_partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
        true -> lists_partition(Pred, T, [H | As], Bs);
        false -> lists_partition(Pred, T, As, [H | Bs])
    end;
lists_partition(_Pred, [], As, Bs) ->
    {lists:reverse(As), lists:reverse(Bs)}.

lists_min([H | T]) -> lists_min(T, H).

lists_min([H | T], Min) when H < Min -> lists_min(T, H);
lists_min([_ | T], Min) -> lists_min(T, Min);
lists_min([], Min) -> Min.
