-module(meshcore_server_core).
-moduledoc false.

%%
%% Functional core for `meshcore_server'.
%%
%% Pure: no I/O, no clock, no randomness. The gen_server (`meshcore_server')
%% injects everything impure as data through an `Env' map (`wall_s' for the
%% advert timestamp, `mono_ms' + `rand22' for the tx back-off) and runs the
%% returned effects. Transmissions are appended to `tx_queue' in the returned
%% state; other side effects (`set_timer', `deliver') flow back as an `Effects'
%% list the gen_server runs.
%%
%% The node's only active job today is to
%% broadcast a signed flood ADVERT every 60 s.
%%

-include_lib("mesh_trace.hrl").

-export([
    init/1,
    handle_rx/4,
    handle_periodic/2,
    enrich/2,
    take_due/2,
    take_one_due/2,
    has_due/2,
    next_wakeup/2,
    handle_tx_results/3
]).

-export_type([core_state/0, effect/0, env/0]).

-record(core, {
    public_key :: binary() | undefined,
    private_key :: binary() | undefined,
    name :: binary() | undefined,
    channel_key :: binary(),
    tx_queue = [] :: [tx_intent()],
    %% discover-reply rate limiter: {window_start_ms, count}
    discover_rl = {0, 0} :: {integer(), non_neg_integer()},
    %% modulation params, for the airtime-derived discover-reply jitter
    spreading_factor = 11 :: pos_integer(),
    bandwidth_hz = 250000 :: pos_integer(),
    coding_rate = 5 :: 5..8,
    preamble_length = 16 :: pos_integer()
}).

-opaque core_state() :: #core{}.

-type effect() ::
    {deliver, Packet :: map()}
    | {set_timer, Ms :: non_neg_integer(), Msg :: term()}.

-type env() :: #{
    wall_s => non_neg_integer(),
    mono_ms => integer(),
    rand22 => non_neg_integer()
}.

-type tx_intent() :: #{
    payload := binary(),
    not_before := now | integer(),
    attempts => non_neg_integer()
}.

-define(INITIAL_PERIODIC_MS, 500).
-define(PERIODIC_MS, 60000).

-define(TX_BACKOFF_BASE_MS, 250).
-define(TX_BACKOFF_MAX_SHIFT, 4).

-define(NODE_TYPE, chat).
-define(SNR_UNKNOWN, -128).
-define(DISCOVER_RL_MAX, 4).
-define(DISCOVER_RL_WINDOW_MS, 120000).

-define(DISCOVER_SLOTS, 5).
-define(DISCOVER_DELAY_WIDEN, 4).

%%------------------------------------------------------------------------------
%% Construction
%%------------------------------------------------------------------------------

-spec init(proplists:proplist()) -> {core_state(), [effect()]}.
init(Opts) ->
    Core = #core{
        public_key = proplists:get_value(public_key, Opts),
        private_key = proplists:get_value(private_key, Opts),
        name = proplists:get_value(name, Opts),
        channel_key = proplists:get_value(
            channel_key, Opts, meshcore_protocol:default_public_channel_key()
        ),
        spreading_factor = proplists:get_value(spreading_factor, Opts, 11),
        bandwidth_hz = proplists:get_value(bandwidth_hz, Opts, 250000),
        coding_rate = coding_rate_denominator(proplists:get_value(coding_rate, Opts)),
        preamble_length = proplists:get_value(preamble_length, Opts, 16)
    },
    {Core, periodic_effects(Core)}.

coding_rate_denominator(cr_4_5) -> 5;
coding_rate_denominator(cr_4_6) -> 6;
coding_rate_denominator(cr_4_7) -> 7;
coding_rate_denominator(cr_4_8) -> 8;
coding_rate_denominator(_) -> 5.

periodic_effects(Core) ->
    case can_advertise(Core) of
        true -> [{set_timer, ?INITIAL_PERIODIC_MS, periodic}];
        false -> []
    end.

can_advertise(#core{public_key = Pub, private_key = Priv, name = Name}) ->
    is_binary(Pub) andalso is_binary(Priv) andalso is_binary(Name).

%%------------------------------------------------------------------------------
%% Receive
%%------------------------------------------------------------------------------

%% Enrich a parsed inbound frame and deliver it with the radio rssi/snr merged
%% in. No forwarding or storage yet.
-spec handle_rx(map(), map(), env(), core_state()) -> {ok, core_state(), [effect()]}.
handle_rx(Packet, Attributes, Env, #core{channel_key = ChannelKey} = Core0) ->
    Delivered = with_attributes(enrich_rx(Packet, ChannelKey, Core0), Attributes),
    Core1 = maybe_reply_discover(Packet, Attributes, Env, Core0),
    {ok, Core1, [{deliver, Delivered}]}.

enrich_rx(#{type := anon_req} = Packet, _ChannelKey, Core) ->
    enrich_anon(Packet, Core);
enrich_rx(Packet, ChannelKey, _Core) ->
    enrich(Packet, ChannelKey).

with_attributes(Packet, Attributes) ->
    Packet#{
        rssi => maps:get(rssi, Attributes, undefined),
        snr => maps:get(snr, Attributes, undefined)
    }.

%% Enrich a parsed frame in place: decrypt group text (ciphertext -> text) or
%% attach the advert signature check; other types pass through unchanged.
-spec enrich(map(), binary()) -> map().
enrich(#{type := grp_txt} = Packet, Key) ->
    case meshcore_protocol:decrypt(Packet, Key) of
        {ok, Decrypted} -> Decrypted;
        {error, Reason} -> Packet#{decrypt_error => Reason}
    end;
enrich(#{type := advert} = Packet, _Key) ->
    Packet#{sig_ok => meshcore_protocol:verify_advert(Packet)};
enrich(Packet, _Key) ->
    Packet.

enrich_anon(
    #{dest_hash := DestHash, sender_pubkey := SenderPub} = Packet,
    #core{public_key = <<OurFirst, _/binary>>, private_key = Priv}
) when is_binary(Priv), DestHash =:= OurFirst ->
    case meshcore_protocol:shared_secret(Priv, SenderPub) of
        {ok, Secret} ->
            case meshcore_protocol:decrypt_shared(Secret, Packet) of
                {ok, Decrypted} -> Decrypted;
                {error, Reason} -> Packet#{decrypt_error => Reason}
            end;
        {error, Reason} ->
            Packet#{decrypt_error => Reason}
    end;
enrich_anon(Packet, _Core) ->
    Packet.

%%------------------------------------------------------------------------------
%% Periodic advert
%%------------------------------------------------------------------------------

%% Periodic tick: enqueue a freshly-signed flood ADVERT and re-arm the 60 s
%% timer. With no identity, do nothing.
-spec handle_periodic(env(), core_state()) -> {core_state(), [effect()]}.
handle_periodic(#{wall_s := NowS}, Core) ->
    case can_advertise(Core) of
        true ->
            {enqueue_advert(NowS, Core), [{set_timer, ?PERIODIC_MS, periodic}]};
        false ->
            ?MESH_TRACE("[meshcore] periodic skip (no identity)~n", []),
            {Core, []}
    end.

enqueue_advert(NowS, #core{public_key = Pub, private_key = Priv, name = Name} = Core) ->
    AppData = meshcore_protocol:encode_advert_appdata(#{node_type => ?NODE_TYPE, name => Name}),
    Advert = meshcore_protocol:sign_advert(
        #{
            route => flood,
            type => advert,
            version => 0,
            hash_size => 1,
            path => <<>>,
            public_key => Pub,
            timestamp => NowS,
            appdata => AppData
        },
        Priv
    ),
    Payload = meshcore_protocol:serialize(Advert),
    ?MESH_TRACE("[meshcore] tx advert name=~p ts=~p bytes=~p~n", [Name, NowS, byte_size(Payload)]),
    enqueue_tx(Payload, now, Core).

%%------------------------------------------------------------------------------
%% Discover reply
%%------------------------------------------------------------------------------

%% Answer a matching network-scan DISCOVER_REQ with a rate-limited, zero-hop
%% DIRECT DISCOVER_RESP.
maybe_reply_discover(
    #{
        type := control,
        sub_type := discover_req,
        type_filter := Filter,
        tag := Tag,
        prefix_only := PrefixOnly
    },
    Attributes,
    Env,
    #core{public_key = Pub} = Core
) when is_binary(Pub) ->
    NowMs = maps:get(mono_ms, Env),
    case rl_allow(NowMs, Core#core.discover_rl) of
        {false, Rl} ->
            ?MESH_TRACE("[meshcore] discover reply rate-limited tag=~p~n", [Tag]),
            Core#core{discover_rl = Rl};
        {true, Rl} ->
            case responds_to(Filter, ?NODE_TYPE) of
                false ->
                    ?MESH_TRACE("[meshcore] discover skip tag=~p filter=~p~n", [Tag, Filter]),
                    Core#core{discover_rl = Rl};
                true ->
                    Payload = discover_resp_payload(Tag, PrefixOnly, Attributes, Pub),
                    Rand = maps:get(rand22, Env),
                    Delay = discover_reply_delay(byte_size(Payload), Rand, Core),
                    ?MESH_TRACE(
                        "[meshcore] discover reply tag=~p snr=~p delay=~pms~n",
                        [Tag, maps:get(snr, Attributes, undefined), Delay]
                    ),
                    enqueue_tx(Payload, NowMs + Delay, Core#core{discover_rl = Rl})
            end
    end;
maybe_reply_discover(_Packet, _Attributes, _Env, Core) ->
    Core.

%% Build + serialize a DISCOVER_RESP: our node type, the SNR we heard the request
%% at (x4), the echoed tag, and our key (8-byte prefix when the request asks).
discover_resp_payload(Tag, PrefixOnly, Attributes, Pub) ->
    Key =
        case PrefixOnly of
            true ->
                <<Prefix:8/binary, _/binary>> = Pub,
                Prefix;
            false ->
                Pub
        end,
    Resp = #{
        route => direct,
        type => control,
        version => 0,
        hash_size => 1,
        path => <<>>,
        sub_type => discover_resp,
        node_type => ?NODE_TYPE,
        reported_snr => snr_byte(maps:get(snr, Attributes, undefined)),
        tag => Tag,
        public_key => Key
    },
    meshcore_protocol:serialize(Resp).

%% Respond only when the request's type filter selects our node type.
responds_to(Filter, NodeType) ->
    Filter band (1 bsl meshcore_protocol:node_type_to_int(NodeType)) =/= 0.

%% Fixed-window rate limiter: count within the current window and deny past the
%% max; once it lapses, open a fresh window.
rl_allow(NowMs, {Start, Count}) when NowMs < Start + ?DISCOVER_RL_WINDOW_MS ->
    {Count + 1 =< ?DISCOVER_RL_MAX, {Start, Count + 1}};
rl_allow(NowMs, _Window) ->
    {true, {NowMs, 1}}.

%% Encode an SNR (dB) as the signed x4 wire byte.
snr_byte(undefined) -> ?SNR_UNKNOWN;
snr_byte(Snr) -> clamp(trunc(Snr * 4), -127, 127).

clamp(V, Lo, _Hi) when V < Lo -> Lo;
clamp(V, _Lo, Hi) when V > Hi -> Hi;
clamp(V, _Lo, _Hi) -> V.

%% Airtime-proportional reply jitter, spreading co-located responders so CAD
%% can de-conflict them.
discover_reply_delay(WireBytes, Rand, Core) ->
    Airtime = packet_airtime_ms(WireBytes, Core),
    T = (Airtime * 52 div 50) div 2,
    (Rand rem ?DISCOVER_SLOTS) * T * ?DISCOVER_DELAY_WIDEN.

%% LoRa time-on-air (ms) of a WireBytes frame under the configured modulation.
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

%%------------------------------------------------------------------------------
%% TX queue ADT (a plain FIFO list behind these accessors)
%%------------------------------------------------------------------------------

enqueue_tx(Payload, NotBefore, Core) ->
    enqueue_intent(#{payload => Payload, not_before => NotBefore}, Core).

enqueue_intent(Intent, #core{tx_queue = Queue} = Core) ->
    Core#core{tx_queue = Queue ++ [Intent]}.

%% Pop every due intent (FIFO), removing it from the queue. Used by tests to
%% drain the queue; the shell pumps with take_one_due/2.
-spec take_due(core_state(), integer()) -> {[tx_intent()], core_state()}.
take_due(#core{tx_queue = Queue} = Core, Now) ->
    {Due, Pending} = lists_partition(fun(Intent) -> is_due(Intent, Now) end, Queue),
    {Due, Core#core{tx_queue = Pending}}.

is_due(#{not_before := now}, _Now) -> true;
is_due(#{not_before := NotBefore}, Now) -> NotBefore =< Now.

%% Pop one due intent, leaving the rest (order preserved). The shell pumps one
%% at a time so RX interleaves between TXs.
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

%% Delay (ms) until the earliest not-yet-due intent, or `infinity' if none.
%% Call after take_due/2 has drained the due intents.
-spec next_wakeup(core_state(), integer()) -> non_neg_integer() | infinity.
next_wakeup(#core{tx_queue = Queue}, Now) ->
    case [NotBefore || #{not_before := NotBefore} <- Queue, is_integer(NotBefore)] of
        [] -> infinity;
        Deadlines -> max(0, lists_min(Deadlines) - Now)
    end.

%% Feed each pumped intent's result back: `ok' is a no-op (take dropped it),
%% `{error, payload_too_large}' is a permanent drop, any other error is transient
%% (re-enqueue with back-off, unbounded retry). The batch shares one `rand22',
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
    ?MESH_TRACE("[meshcore] tx drop (payload_too_large) bytes=~p~n", [
        byte_size(maps:get(payload, _Intent))
    ]),
    handle_tx_results(Rest, Env, Core, Index + 1);
handle_tx_results(
    [{Intent, {error, _Reason}} | Rest], Env, #core{tx_queue = Queue} = Core, Index
) ->
    #{mono_ms := Now, rand22 := Rand} = Env,
    Attempts = maps:get(attempts, Intent, 0),
    Seed = Rand bxor (Index bsl 16) bxor (Attempts bsl 8),
    Backoff = backoff_ms(Attempts, Seed),
    ?MESH_TRACE("[meshcore] tx retry (~p) attempt=~p in ~pms~n", [_Reason, Attempts + 1, Backoff]),
    Intent1 = Intent#{not_before => Now + Backoff, attempts => Attempts + 1},
    handle_tx_results(Rest, Env, Core#core{tx_queue = Queue ++ [Intent1]}, Index + 1).

-spec backoff_ms(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
backoff_ms(Attempts, Rand) ->
    Window = ?TX_BACKOFF_BASE_MS bsl min(Attempts, ?TX_BACKOFF_MAX_SHIFT),
    Rand rem Window.

%%------------------------------------------------------------------------------
%% AtomVM stdlib gaps
%%
%% TODO: drop these and call lists:partition/2 and lists:min/1 directly once
%% AtomVM exposes them.
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
