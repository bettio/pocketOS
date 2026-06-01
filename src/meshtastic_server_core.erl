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
    rx_needs_peer_key/2,
    next_packet_id/2,
    take_due/2,
    handle_tx_results/3,
    next_wakeup/2,
    rebroadcast_delay_ms/2,
    cw_size/1,
    update_last_seen/4,
    prune_expired_last_seen/2
]).

-export_type([core_state/0, effect/0, env/0]).

-record(core, {
    node_id :: non_neg_integer(),
    channel :: map(),
    private_key :: binary() | undefined,
    node_info :: map() | undefined,
    rolling_packet_id :: non_neg_integer(),
    last_seen = #{} :: map(),
    tx_queue = [] :: [tx_intent()]
}).

-opaque core_state() :: #core{}.

-type effect() ::
    {deliver, DecodedPacket :: map()}
    | {set_timer, Ms :: non_neg_integer(), Msg :: term()}.

%% - `now_ms` is the one injected clock: monotonic milliseconds, used for
%% rebroadcast / back-off / tx-scheduling deadlines.
%% - `rand22` is a fresh 22-bit value (top bits of an originated packet_id)
%% - `peer_key` is present only when rx_needs_peer_key/2 told the gen_server to resolve
%% it for a PKI packet addressed to us.
-type env() :: #{
    now_ms => integer(),
    rand22 => non_neg_integer(),
    peer_key => {ok, binary()} | {error, term()}
}.

%% A queued transmission. `not_before` is `now` or an absolute monotonic-ms
%% deadline (SNR-weighted rebroadcast delay, or back-off after a failed send).
%% `attempts` sizes the back-off, never bounds retries; `ref` = {Src, PacketId}
%% tags a relay so an overheard duplicate can cancel it.
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
-define(DEFAULT_NODE_ID, 1127302788).
-define(INITIAL_PERIODIC_MS, 500).
-define(PERIODIC_MS, 60000).

%% Channel-access back-off (CSMA): re-enqueue a failed send at now + rand(0, window) ms,
%% window doubling per attempt up to 250 bsl 4 = 4000 ms. Unbounded.
-define(TX_BACKOFF_BASE_MS, 250).
-define(TX_BACKOFF_MAX_SHIFT, 4).

%% Defer each rebroadcast by `2*CWmax*slot + random(0, 2^CWsize)*slot`, where
%% CWsize maps our reception SNR (?SNR_MIN..?SNR_MAX dB) onto [?CW_MIN, ?CW_MAX],
%% so a weakly-heard (far) packet relays first -> range extension. We are role
%% CLIENT, hence the non-router branch (with the 2*CWmax*slot floor).
%%
%% TODO: the constants below are HARDCODED for SF9/BW250 medium-fast config.
%% They must be revisited whenever the modulation / preset changes.
%% ?REBROADCAST_SLOT_MS especially: the slot is a function of
%% SF/BW, so it changes with every preset. Eventually these should be computed
%% from the active radio config instead of living here as literals.
%%
%% Slot = airtime of one CAD / back-off unit:
%%   max(2.25, NUM_SYM_CAD+0.5) * 2^SF/BW + 7.6 ms
%% Examples (BW250):
%%   SF9  -> 2.5 * 2.048 + 7.6 ~= 13 ms
%%   SF11 -> 2.5 * 8.192 + 7.6 ~= 28 ms
-define(CW_MIN, 3).
-define(CW_MAX, 8).
-define(SNR_MIN, -20).
-define(SNR_MAX, 10).
-define(REBROADCAST_SLOT_MS, 13).

%%------------------------------------------------------------------------------
%% Construction
%%------------------------------------------------------------------------------

-spec init(proplists:proplist(), non_neg_integer()) -> {core_state(), [effect()]}.
init(MeshtasticOpts, InitialRolling) ->
    NodeId = proplists:get_value(node_id, MeshtasticOpts, ?DEFAULT_NODE_ID),
    NodeInfo = proplists:get_value(node_info, MeshtasticOpts),
    Channel = init_channel(proplists:get_value(channel, MeshtasticOpts)),
    PrivateKey = proplists:get_value(private_key, MeshtasticOpts),
    ?MESH_TRACE("[mesh] init node_id=~p initial_rolling=~p~n", [NodeId, InitialRolling]),
    Core = #core{
        node_id = NodeId,
        channel = Channel,
        private_key = PrivateKey,
        node_info = NodeInfo,
        rolling_packet_id = InitialRolling
    },
    {Core, [{set_timer, ?INITIAL_PERIODIC_MS, periodic}]}.

init_channel(undefined) ->
    meshtastic:default_long_fast_channel();
init_channel(#{name := Name, psk := Psk} = Channel) ->
    Channel#{hash => meshtastic:channel_hash(Name, Psk)}.

%%------------------------------------------------------------------------------
%% Receive
%%------------------------------------------------------------------------------

%% Handle a parsed inbound packet. The caller has already deserialized the wire
%% bytes (a parse failure never reaches here) and, for PKI packets to us,
%% pre-resolved `Env.peer_key`. Returns the reply atom the radio_manager
%% dispatch expects (`ok` | `discard`), the updated state (with `tx_queue`
%% already populated for any reply/rebroadcast), and the transient effects.
-spec handle_rx(map(), map(), env(), core_state()) -> {ok | discard, core_state(), [effect()]}.
handle_rx(#{src := Src}, _Attributes, _Env, #core{node_id = Src} = Core) ->
    ?MESH_TRACE("[mesh] rx discard (from this node)~n", []),
    {discard, Core, []};
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
            Core1 = enqueue_rebroadcast(
                Packet, Attributes, Env, Core#core{last_seen = PrunedLastSeen}
            ),
            {ok, Core1, []};
        true ->
            ?MESH_TRACE("[mesh] rx duplicate src=~p pid=~p~n", [Src, PacketId]),
            {discard, Core, []}
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
                    Core2 = enqueue_replies(Packet, Message, Env, Core1),
                    Core3 = enqueue_rebroadcast(Packet, Attributes, Env, Core2),
                    {ok, Core3, [{deliver, DecodedPacket}]}
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

%% At most one reply is produced: a NodeInfo want_response reply takes
%% precedence over (and replaces) a ROUTING ACK.
enqueue_replies(Packet, Message, Env, Core) ->
    case node_info_reply(Packet, Message, Env, Core) of
        {true, Core1} -> Core1;
        {false, Core1} -> ack_reply(Packet, Message, Env, Core1)
    end.

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
    #{src := Src, dest := Dest, want_ack := true, packet_id := OrigPid},
    Message,
    Env,
    #core{node_id = Dest} = Core
) when Dest =/= ?BROADCAST_ADDR ->
    case maps:is_key(request_id, Message) of
        true ->
            %% Don't ACK an ACK (a packet that is itself a response).
            Core;
        false ->
            AckData = #{
                portnum => 'ROUTING_APP',
                payload => #{error_reason => 'NONE'},
                request_id => OrigPid
            },
            AckBin = erlang:iolist_to_binary(meshtastic_proto:encode(AckData)),
            ?MESH_TRACE("[mesh] ack pid=~p -> src=~p~n", [OrigPid, Src]),
            originate(Src, AckBin, Env, Core)
    end;
ack_reply(_Packet, _Message, _Env, Core) ->
    Core.

%% Rebroadcast the original (still-encrypted) packet with hop_limit decremented,
%% next_hop cleared (we don't learn routes, so a directed packet falls back to
%% flooding) and relay_node stamped to our last byte. Skipped for unicasts to
%% us, exhausted hop limits, and packets directed at another named hop.
enqueue_rebroadcast(#{dest := Dest} = _Packet, _Attributes, _Env, #core{node_id = Dest} = Core) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (unicast-to-us)~n", []),
    Core;
enqueue_rebroadcast(#{hop_limit := 0} = _Packet, _Attributes, _Env, Core) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (hop=0)~n", []),
    Core;
enqueue_rebroadcast(
    #{hop_limit := HopLimit, next_hop := NextHop, src := Src, packet_id := PacketId} = Packet,
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
            RadioPayload = meshtastic:serialize(Packet#{
                hop_limit := HopLimit - 1,
                next_hop := 0,
                relay_node := RelayByte
            }),
            Snr = maps:get(snr, Attributes, undefined),
            Delay = rebroadcast_delay_ms(Snr, maps:get(rand22, Env)),
            NotBefore = maps:get(now_ms, Env) + Delay,
            ?MESH_TRACE(
                "[mesh] rebroadcast pid=~p hop=~p->~p relay=~p snr=~p in ~pms~n",
                [PacketId, HopLimit, HopLimit - 1, RelayByte, Snr, Delay]
            ),
            enqueue_intent(
                #{payload => RadioPayload, not_before => NotBefore, ref => {Src, PacketId}},
                Core
            )
    end.

%%------------------------------------------------------------------------------
%% Send / periodic
%%------------------------------------------------------------------------------

-spec handle_send(non_neg_integer(), binary(), env(), core_state()) ->
    {ok, core_state(), [effect()]}.
handle_send(Dest, Data, Env, Core) ->
    {ok, originate(Dest, Data, Env, Core), []}.

-spec handle_periodic(env(), core_state()) -> {core_state(), [effect()]}.
handle_periodic(Env, #core{node_info = #{user_info := _UserInfo}} = Core) ->
    ?MESH_TRACE("[mesh] periodic node_info broadcast~n", []),
    Core1 = send_node_info(?BROADCAST_ADDR, #{}, Env, Core),
    {Core1, [{set_timer, ?PERIODIC_MS, periodic}]};
handle_periodic(_Env, Core) ->
    ?MESH_TRACE("[mesh] periodic skip (no user_info)~n", []),
    {Core, []}.

send_node_info(Dest, Extra, Env, #core{node_info = #{user_info := UserInfo}} = Core) ->
    Data = maps:merge(#{portnum => 'NODEINFO_APP', payload => UserInfo}, Extra),
    Bin = erlang:iolist_to_binary(meshtastic_proto:encode(Data)),
    originate(Dest, Bin, Env, Core);
send_node_info(_Dest, _Extra, _Env, Core) ->
    ?MESH_TRACE("[mesh] node_info send skip (no user_info)~n", []),
    Core.

%% Build a freshly-originated packet (new packet_id from the rolling counter +
%% Env.rand22), encrypt with the channel PSK, serialize and enqueue for TX.
originate(Dest, Data, Env, #core{
    node_id = NodeId,
    rolling_packet_id = Rolling,
    channel = #{psk := Psk, hash := ChannelHash}
} = Core) ->
    Rand22 = maps:get(rand22, Env),
    {PacketId, NextRolling} = next_packet_id(Rolling, Rand22),
    Packet = #{
        dest => Dest,
        src => NodeId,
        packet_id => PacketId,
        hop_start => 3,
        via_mqtt => false,
        want_ack => false,
        hop_limit => 3,
        channel_hash => ChannelHash,
        next_hop => 0,
        relay_node => meshtastic:relay_node_byte(NodeId),
        data => Data
    },
    Encrypted = meshtastic:encrypt(Packet, Psk),
    RadioPayload = meshtastic:serialize(Encrypted),
    ?MESH_TRACE(
        "[mesh] tx pid=~p dest=~p data_bytes=~p wire_bytes=~p~n",
        [PacketId, Dest, byte_size(Data), byte_size(RadioPayload)]
    ),
    enqueue_tx(RadioPayload, now, Core#core{rolling_packet_id = NextRolling}).

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

-spec rebroadcast_delay_ms(integer() | undefined, non_neg_integer()) -> non_neg_integer().
rebroadcast_delay_ms(Snr, Rand) ->
    CWsize = cw_size(Snr),
    Offset = 2 * ?CW_MAX * ?REBROADCAST_SLOT_MS,
    Window = 1 bsl CWsize,
    Offset + (Rand rem Window) * ?REBROADCAST_SLOT_MS.

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
