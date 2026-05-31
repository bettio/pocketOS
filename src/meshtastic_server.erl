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

-export([
    start_link/2,
    start_link/3,
    handle_payload/4,
    send/3
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
    gen_server:call(Server, {send, DestAddr, Data}).

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
            Env0 = #{now => erlang:monotonic_time(second), rand22 => rand22()},
            Env = maybe_resolve_peer_key(Packet, Callbacks, Core0, Env0),
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
    {send, DestAddr, Data},
    _From,
    #state{callbacks = Callbacks, core = Core0} = State
) ->
    Env = #{rand22 => rand22()},
    {ok, Core1, Effects} = meshtastic_server_core:handle_send(DestAddr, Data, Env, Core0),
    run_effects(Effects, Callbacks),
    State1 = pump_tx(State#state{core = Core1}),
    {reply, ok, State1};
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(periodic, #state{callbacks = Callbacks, core = Core0} = State) ->
    Env = #{rand22 => rand22()},
    {Core1, Effects} = meshtastic_server_core:handle_periodic(Env, Core0),
    run_effects(Effects, Callbacks),
    State1 = pump_tx(State#state{core = Core1}),
    {noreply, State1};
handle_info(tx_pump, State) ->
    {noreply, pump_tx(State)};
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
run_effects([{set_timer, Ms, Msg} | Rest], Callbacks) ->
    erlang:send_after(Ms, self(), Msg),
    run_effects(Rest, Callbacks).

deliver(undefined, _DecodedPacket) ->
    ok;
deliver(Callbacks, DecodedPacket) ->
    Callbacks:message_cb(DecodedPacket).

%% Drain every due transmission from the core's tx_queue out to the radio, then
%% schedule a wake-up for the earliest not-yet-due intent (none today, so no
%% timer is armed). The broadcast result is ignored, as before.
pump_tx(#state{radio = {_RadioId, RadioModule, Radio}, core = Core0} = State) ->
    Now = erlang:monotonic_time(millisecond),
    {DuePayloads, Core1} = meshtastic_server_core:take_due(Core0, Now),
    lists:foreach(
        fun(Payload) -> RadioModule:broadcast(Radio, Payload) end,
        DuePayloads
    ),
    case meshtastic_server_core:next_wakeup(Core1, Now) of
        infinity -> ok;
        Delay -> erlang:send_after(Delay, self(), tx_pump)
    end,
    State#state{core = Core1}.

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

rand22() ->
    <<TopRand:22, _:10>> = crypto:strong_rand_bytes(4),
    TopRand.
