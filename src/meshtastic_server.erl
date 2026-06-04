-module(meshtastic_server).

-behavior(gen_server).

%%
%% Imperative gen_server layer around the functional core `meshtastic_server_core`.
%%
%% This module owns the *runtime concerns*: the gen_server process and its
%% mailbox, the radio handle, the application callback module, timers, the
%% clock and randomness. Every callback is thin -- it injects the impurity the
%% core needs (a fresh `rand22`, the monotonic clock, a pre-resolved PKI peer
%% key) as data, calls one pure core function, performs the returned effects,
%% and pumps the core's tx_queue out to the radio. All packet logic lives in
%% the core; see meshtastic_server_core.erl.
%%
%% On-air tested against meshtastic 2.7.15.567b8ea: all implemented features
%% verified working.
%%

-export([
    start_link/2,
    start_link/3,
    handle_payload/4,
    send/3,
    send/4,
    send_async/3,
    send_async/4
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-include_lib("mesh_trace.hrl").

-record(state, {
    radio,
    callbacks,
    core
}).

start_link(Radio, MeshtasticOpts) ->
    gen_server:start_link(?MODULE, [Radio, MeshtasticOpts], []).

start_link(Name, Radio, MeshtasticOpts) ->
    gen_server:start_link(Name, ?MODULE, [Radio, MeshtasticOpts], []).

handle_payload(Server, {_IfaceId, _Pid} = Iface, Payload, Attributes) ->
    gen_server:call(Server, {handle_payload, Iface, Payload, Attributes}).

send(Server, DestAddr, Data) ->
    send(Server, DestAddr, Data, #{}).

send(Server, DestAddr, Data, Opts) ->
    gen_server:call(Server, {send, DestAddr, Data, Opts}).

%% Delivery-tracked send/4: replies {ok, Ref} and later sends the caller a
%% {delivery_update, Ref, meshtastic_server_core:delivery_status()} message.
%% TODO: process aliases (missing on AtomVM) would let a gone caller opt out.
-spec send_async(gen_server:server_ref(), non_neg_integer(), binary()) ->
    {ok, reference()} | {error, term()}.
send_async(Server, DestAddr, Data) ->
    send_async(Server, DestAddr, Data, #{}).

-spec send_async(gen_server:server_ref(), non_neg_integer(), binary(), map()) ->
    {ok, reference()} | {error, term()}.
send_async(Server, DestAddr, Data, Opts) ->
    gen_server:call(Server, {send_async, DestAddr, Data, Opts}).

init([Radio, MeshtasticOpts]) ->
    Callbacks = proplists:get_value(callbacks, MeshtasticOpts),
    <<InitialRolling:32>> = crypto:strong_rand_bytes(4),
    {Core, Effects} = meshtastic_server_core:init(MeshtasticOpts, InitialRolling),
    run_effects(Effects, Callbacks),
    {ok, #state{radio = Radio, callbacks = Callbacks, core = Core}}.

handle_call(
    {handle_payload, {_IfaceId, _Pid}, Payload, Attributes},
    _From,
    #state{callbacks = Callbacks, core = Core0} = State
) ->
    case meshtastic:parse(Payload) of
        {ok, Packet} ->
            Env0 = #{now_ms => erlang:monotonic_time(millisecond), rand22 => rand22()},
            Env1 = maybe_resolve_peer_key(Packet, Callbacks, Core0, Env0),
            Env = Env1#{
                routes => resolve_routes(
                    meshtastic_server_core:rx_route_dests(Packet, Core0), Callbacks
                )
            },
            {Reply, Core1, Effects} = meshtastic_server_core:handle_rx(
                Packet, Attributes, Env, Core0
            ),
            run_effects(Effects, Callbacks),
            State1 = pump_tx(State#state{core = Core1}),
            {reply, Reply, State1};
        _SomethingElse ->
            ?MESH_TRACE("[mesh] rx parse-failed: ~p~n", [_SomethingElse]),
            {reply, next, State}
    end;
handle_call(
    {send, DestAddr, Data, Opts},
    _From,
    #state{callbacks = Callbacks, core = Core0} = State
) ->
    case prepare_send_env(DestAddr, Opts, Callbacks, Core0) of
        {ok, Env} ->
            {ok, Core1, Effects} = meshtastic_server_core:handle_send(DestAddr, Data, Env, Core0),
            run_effects(Effects, Callbacks),
            State1 = pump_tx(State#state{core = Core1}),
            {reply, ok, State1};
        {error, _Reason} = Error ->
            ?MESH_TRACE("[mesh] send aborted dest=~p reason=~p~n", [DestAddr, _Reason]),
            {reply, Error, State}
    end;
handle_call(
    {send_async, DestAddr, Data, Opts},
    {Pid, _Tag},
    #state{callbacks = Callbacks, core = Core0} = State
) ->
    case prepare_send_env(DestAddr, Opts, Callbacks, Core0) of
        {ok, Env} ->
            Ref = make_ref(),
            {ok, Core1, Effects} = meshtastic_server_core:handle_send(
                DestAddr, Data, Env#{notify => {Pid, Ref}}, Core0
            ),
            run_effects(Effects, Callbacks),
            State1 = pump_tx(State#state{core = Core1}),
            {reply, {ok, Ref}, State1};
        {error, _Reason} = Error ->
            ?MESH_TRACE("[mesh] send_async aborted dest=~p reason=~p~n", [DestAddr, _Reason]),
            {reply, Error, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(periodic, #state{callbacks = Callbacks, core = Core0} = State) ->
    Env = #{rand22 => rand22(), routes => #{}},
    {Core1, Effects} = meshtastic_server_core:handle_periodic(Env, Core0),
    run_effects(Effects, Callbacks),
    State1 = pump_tx(State#state{core = Core1}),
    {noreply, State1};
handle_info(tx_pump, State) ->
    {noreply, pump_tx(State)};
handle_info({ack_timeout, PacketId}, #state{callbacks = Callbacks, core = Core0} = State) ->
    {Core1, Effects} = meshtastic_server_core:handle_ack_timeout(PacketId, Core0),
    run_effects(Effects, Callbacks),
    State1 = pump_tx(State#state{core = Core1}),
    {noreply, State1};
handle_info(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Effect execution
%%------------------------------------------------------------------------------

run_effects([], _Callbacks) ->
    ok;
run_effects([{deliver, DecodedPacket} | Rest], Callbacks) ->
    deliver(Callbacks, DecodedPacket),
    run_effects(Rest, Callbacks);
run_effects([{learn, NodeId, NextHop} | Rest], Callbacks) ->
    learn_route(Callbacks, NodeId, NextHop),
    run_effects(Rest, Callbacks);
run_effects([{set_timer, Ms, Msg} | Rest], Callbacks) ->
    erlang:send_after(Ms, self(), Msg),
    run_effects(Rest, Callbacks);
run_effects([{notify, {Pid, Ref}, Status} | Rest], Callbacks) ->
    Pid ! {delivery_update, Ref, Status},
    run_effects(Rest, Callbacks).

deliver(undefined, _DecodedPacket) ->
    ok;
deliver(Callbacks, DecodedPacket) ->
    Callbacks:message_cb(DecodedPacket).

learn_route(undefined, _NodeId, _NextHop) ->
    ok;
learn_route(Callbacks, NodeId, NextHop) ->
    Callbacks:learn_route(NodeId, NextHop).

%% Send at most ONE due intent per call, then re-arm tx_pump and return to the
%% loop so RX interleaves between TXs; the core re-enqueues failed sends with a
%% back-off.
pump_tx(#state{radio = {_RadioId, RadioModule, Radio}, core = Core0} = State) ->
    Now = erlang:monotonic_time(millisecond),
    case meshtastic_server_core:take_one_due(Core0, Now) of
        {none, Core1} ->
            arm_tx_pump(Core1, Now),
            State#state{core = Core1};
        {#{payload := Payload} = Intent, Core1} ->
            Result = RadioModule:broadcast(Radio, Payload),
            NowAfter = erlang:monotonic_time(millisecond),
            Env = #{now_ms => NowAfter, rand22 => rand22()},
            Core2 = meshtastic_server_core:handle_tx_results([{Intent, Result}], Env, Core1),
            arm_tx_pump(Core2, NowAfter),
            State#state{core = Core2}
    end.

arm_tx_pump(Core, Now) ->
    Delay =
        case meshtastic_server_core:has_due(Core, Now) of
            true -> 0;
            false -> meshtastic_server_core:next_wakeup(Core, Now)
        end,
    case Delay of
        infinity -> ok;
        D -> erlang:send_after(D, self(), tx_pump)
    end.

%%------------------------------------------------------------------------------
%% PKI peer-key pre-resolution
%%------------------------------------------------------------------------------

%% Resolve the peer's public key (impure: reads the application node db via the
%% callback) only when the core says this packet is a PKI unicast to us. The
%% result is handed to the core as data so decryption itself stays pure.
maybe_resolve_peer_key(Packet, Callbacks, Core, Env) ->
    case meshtastic_server_core:rx_needs_peer_key(Packet, Core) of
        true -> Env#{peer_key => pre_resolve_peer_key(Packet, Callbacks)};
        false -> Env
    end.

pre_resolve_peer_key(_Packet, undefined) ->
    {error, no_callbacks};
pre_resolve_peer_key(#{src := NodeId}, Callbacks) ->
    try Callbacks:peer_public_key(NodeId) of
        {ok, _} = Result -> Result;
        Other -> {error, {peer_pubkey_lookup, Other}}
    catch
        _:_ -> {error, peer_pubkey_lookup_failed}
    end.

%%------------------------------------------------------------------------------
%% Send env preparation
%%------------------------------------------------------------------------------

prepare_send_env(DestAddr, #{pki := true}, Callbacks, Core) ->
    case meshtastic_server_core:can_send_pki(Core) of
        false ->
            {error, no_private_key};
        true ->
            case pre_resolve_peer_key(#{src => DestAddr}, Callbacks) of
                {ok, _} = PeerKey ->
                    {ok, (base_send_env(DestAddr, Callbacks))#{pki => true, peer_key => PeerKey}};
                {error, _} = Error ->
                    Error
            end
    end;
prepare_send_env(DestAddr, _Opts, Callbacks, _Core) ->
    {ok, base_send_env(DestAddr, Callbacks)}.

base_send_env(DestAddr, Callbacks) ->
    #{rand22 => rand22(), routes => resolve_routes([DestAddr], Callbacks)}.

%%------------------------------------------------------------------------------
%% Next-hop route resolution
%%------------------------------------------------------------------------------

resolve_routes([], _Callbacks) ->
    #{};
resolve_routes(_Dests, undefined) ->
    #{};
resolve_routes(Dests, Callbacks) ->
    Callbacks:next_hop_routes(Dests).

rand22() ->
    <<TopRand:22, _:10>> = crypto:strong_rand_bytes(4),
    TopRand.
