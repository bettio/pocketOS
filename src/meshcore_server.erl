-module(meshcore_server).
-behavior(gen_server).

%%
%% Imperative gen_server layer around the functional core `meshcore_server_core'.
%%
%% Thin shell: it owns the gen_server process and its mailbox, the radio handle,
%% timers, the clock and randomness. Each callback injects the impurity the core
%% needs as data, calls one pure core function, runs the returned effects, and
%% pumps the core's tx_queue out to the radio. All packet logic lives in the
%% core; see meshcore_server_core.erl. Delivered frames are logged and handed
%% to the optional `callbacks' module.
%%

-export([
    start_link/2,
    start_link/3,
    handle_payload/4
]).

%% Pure presentation helper, exported for tests.
-export([
    for_log/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {radio, callbacks, core}).

start_link(Radio, Opts) ->
    gen_server:start_link(?MODULE, [Radio, Opts], []).

start_link(Name, Radio, Opts) ->
    gen_server:start_link(Name, ?MODULE, [Radio, Opts], []).

handle_payload(Server, {_IfaceId, _Pid} = Iface, Payload, Attributes) ->
    gen_server:call(Server, {handle_payload, Iface, Payload, Attributes}).

init([Radio, Opts]) ->
    erlang:display(
        {meshcore_crypto, eddsa_available, meshcore_protocol:eddsa_available(), crypto:info_lib()}
    ),
    Callbacks = proplists:get_value(callbacks, Opts),
    {Core, Effects} = meshcore_server_core:init(Opts),
    run_effects(Effects, Callbacks),
    {ok, #state{radio = Radio, callbacks = Callbacks, core = Core}}.

handle_call(
    {handle_payload, {_IfaceId, _Pid}, Payload, Attributes},
    _From,
    #state{callbacks = Callbacks, core = Core0} = State
) ->
    case meshcore_protocol:parse(Payload) of
        {ok, Packet} ->
            Env = #{mono_ms => erlang:monotonic_time(millisecond), rand22 => rand22()},
            {ok, Core1, Effects} = meshcore_server_core:handle_rx(Packet, Attributes, Env, Core0),
            run_effects(Effects, Callbacks),
            State1 = pump_tx(State#state{core = Core1}),
            {reply, ok, State1};
        _Invalid ->
            {reply, next, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(periodic, #state{callbacks = Callbacks, core = Core0} = State) ->
    Env = #{wall_s => erlang:system_time(second)},
    {Core1, Effects} = meshcore_server_core:handle_periodic(Env, Core0),
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
run_effects([{set_timer, Ms, Msg} | Rest], Callbacks) ->
    erlang:send_after(Ms, self(), Msg),
    run_effects(Rest, Callbacks);
run_effects([{deliver, Packet} | Rest], Callbacks) ->
    erlang:display({meshcore_rx, for_log(Packet)}),
    deliver(Callbacks, Packet),
    run_effects(Rest, Callbacks).

deliver(undefined, _Packet) ->
    ok;
deliver(Callbacks, Packet) ->
    Callbacks:message_cb(Packet).

pump_tx(#state{radio = {_RadioId, RadioModule, Radio}, core = Core0} = State) ->
    Now = erlang:monotonic_time(millisecond),
    case meshcore_server_core:take_one_due(Core0, Now) of
        {none, Core1} ->
            arm_tx_pump(Core1, Now),
            State#state{core = Core1};
        {#{payload := Payload} = Intent, Core1} ->
            Result = RadioModule:broadcast(Radio, Payload),
            NowAfter = erlang:monotonic_time(millisecond),
            Env = #{mono_ms => NowAfter, rand22 => rand22()},
            Core2 = meshcore_server_core:handle_tx_results([{Intent, Result}], Env, Core1),
            arm_tx_pump(Core2, NowAfter),
            State#state{core = Core2}
    end.

arm_tx_pump(Core, Now) ->
    Delay =
        case meshcore_server_core:has_due(Core, Now) of
            true -> 0;
            false -> meshcore_server_core:next_wakeup(Core, Now)
        end,
    case Delay of
        infinity -> ok;
        D -> erlang:send_after(D, self(), tx_pump)
    end.

%%------------------------------------------------------------------------------
%% Presentation
%%------------------------------------------------------------------------------

%% Drop the bulky binaries that are just noise in the serial log; the full map
%% from the core keeps them for storage.
%% TODO: collapse to maps:without/2 once AtomVM exposes it (absent as of 0.7).
for_log(Packet) ->
    P1 = maps:remove(appdata, maps:remove(signature, maps:remove(public_key, Packet))),
    maps:remove(ciphertext, maps:remove(sender_pubkey, P1)).

rand22() ->
    <<TopRand:22, _:10>> = crypto:strong_rand_bytes(4),
    TopRand.
