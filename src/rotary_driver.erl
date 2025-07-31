-module(rotary_driver).

-behavior(gen_server).

-export([
    start_link/0,
    open/2,
    subscribe/2,
    unsubscribe/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    listener = undefined,
    gpio_ref = undefined,
    clk_pin,
    dt_pin,
    sw_pin = undefined,
    % Last valid state {ClkLevel, DtLevel}
    last_state = undefined,
    % Timer reference for debouncing
    debounce_timer = undefined,
    % Debounce time in milliseconds
    debounce_ms = 5
}).

%% Valid state transitions for quadrature decoding
%% Using Gray code: only one bit changes at a time
-define(VALID_TRANSITIONS, #{
    %% From state -> To state -> Direction
    {{0, 0}, {0, 1}} => counter_clockwise,
    {{0, 0}, {1, 0}} => clockwise,
    {{0, 1}, {0, 0}} => clockwise,
    {{0, 1}, {1, 1}} => counter_clockwise,
    {{1, 0}, {0, 0}} => counter_clockwise,
    {{1, 0}, {1, 1}} => clockwise,
    {{1, 1}, {0, 1}} => clockwise,
    {{1, 1}, {1, 0}} => counter_clockwise
}).

%%-----------------------------------------------------------------------------
%% Public API
%%-----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Open encoder with CLK and DT pins only, or with CLK, DT and SW pins
open(Pid, {ClkPin, DtPin}) ->
    gen_server:call(Pid, {open, ClkPin, DtPin, undefined});
open(Pid, {ClkPin, DtPin, SwPin}) ->
    gen_server:call(Pid, {open, ClkPin, DtPin, SwPin}).

%% Subscribe to encoder events
subscribe(Pid, all) ->
    gen_server:call(Pid, {subscribe_input, all}).

%% Unsubscribe from encoder events
unsubscribe(Pid) ->
    gen_server:call(Pid, unsubscribe_input).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({open, ClkPin, DtPin, SwPin}, _From, State) ->
    %% Start GPIO if not already started
    GPIO = gpio:start(),

    %% Configure CLK and DT pins
    ok = gpio:set_direction(GPIO, ClkPin, input),
    ok = gpio:set_direction(GPIO, DtPin, input),
    ok = gpio:set_int(GPIO, ClkPin, both),
    ok = gpio:set_int(GPIO, DtPin, both),

    %% Configure SW pin if provided
    case SwPin of
        undefined ->
            ok;
        _ ->
            ok = gpio:set_direction(GPIO, SwPin, input),
            ok = gpio:set_int(GPIO, SwPin, both)
    end,

    %% Read initial state
    ClkLevel =
        case gpio:read(GPIO, ClkPin) of
            high -> 1;
            low -> 0
        end,
    DtLevel =
        case gpio:read(GPIO, DtPin) of
            high -> 1;
            low -> 0
        end,

    NewState = State#state{
        gpio_ref = GPIO,
        clk_pin = ClkPin,
        dt_pin = DtPin,
        sw_pin = SwPin,
        last_state = {ClkLevel, DtLevel}
    },

    {reply, ok, NewState};
handle_call({subscribe_input, all}, {Pid, _Ref}, State) ->
    {reply, ok, State#state{listener = Pid}};
handle_call(unsubscribe_input, _From, State) ->
    {reply, ok, State#state{listener = undefined}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle GPIO interrupts
handle_info({gpio_interrupt, Pin}, State) when
    Pin =:= State#state.clk_pin orelse
        Pin =:= State#state.dt_pin
->
    handle_encoder_interrupt(State);
handle_info({gpio_interrupt, Pin}, State) when Pin =:= State#state.sw_pin ->
    handle_switch_interrupt(State);
%% Handle debounce timer
handle_info({debounce_timeout, NewState}, State) ->
    process_encoder_state(NewState, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{gpio_ref = GPIO} = _State) when GPIO =/= undefined ->
    %% Clean up GPIO resources
    catch gpio:close(GPIO),
    ok;
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

handle_encoder_interrupt(#state{debounce_timer = Timer} = State) when Timer =/= undefined ->
    %% Debounce timer is active, ignore this interrupt
    {noreply, State};
handle_encoder_interrupt(
    #state{
        gpio_ref = GPIO,
        clk_pin = ClkPin,
        dt_pin = DtPin,
        debounce_ms = DebounceMs
    } = State
) ->
    %% Read current state
    ClkLevel =
        case gpio:read(GPIO, ClkPin) of
            high -> 1;
            low -> 0
        end,
    DtLevel =
        case gpio:read(GPIO, DtPin) of
            high -> 1;
            low -> 0
        end,

    NewState = {ClkLevel, DtLevel},

    %% Start debounce timer
    TimerRef = erlang:send_after(DebounceMs, self(), {debounce_timeout, NewState}),

    {noreply, State#state{debounce_timer = TimerRef}}.

process_encoder_state(
    NewState,
    #state{
        last_state = LastState,
        listener = Listener,
        debounce_timer = Timer
    } = State
) ->
    %% Cancel debounce timer
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,

    %% Check if state actually changed
    case NewState of
        LastState ->
            %% No change, ignore
            {noreply, State#state{debounce_timer = undefined}};
        _ ->
            %% State changed, check if it's a valid transition
            case maps:get({LastState, NewState}, ?VALID_TRANSITIONS, undefined) of
                undefined ->
                    %% Invalid transition, update state but don't send event
                    %% This handles noise and intermediate states
                    {noreply, State#state{last_state = NewState, debounce_timer = undefined}};
                Direction ->
                    %% Valid transition, send event
                    send_event(Listener, {rotary, Direction}),
                    {noreply, State#state{last_state = NewState, debounce_timer = undefined}}
            end
    end.

handle_switch_interrupt(#state{gpio_ref = GPIO, sw_pin = SwPin, listener = Listener} = State) ->
    %% Read switch state
    SwState =
        case gpio:read(GPIO, SwPin) of
            high -> released;
            low -> pressed
        end,

    %% Send switch event
    send_event(Listener, {switch, SwState}),

    {noreply, State}.

send_event(undefined, _Event) ->
    ok;
send_event(Listener, Event) ->
    Timestamp = erlang:system_time(millisecond),
    erlang:display({Listener, {input_event, self(), Timestamp, Event}}),
    case Event of
        {switch, pressed} ->
            Listener ! {input_event, self(), Timestamp, {keyboard, down, $\n}};
        {switch, released} ->
            Listener ! {input_event, self(), Timestamp, {keyboard, up, $\n}};
        {rotary, clockwise} ->
            Listener ! {input_event, self(), Timestamp, {keyboard, down, up}},
            Listener ! {input_event, self(), Timestamp, {keyboard, up, up}};
        {rotary, counter_clockwise} ->
            Listener ! {input_event, self(), Timestamp, {keyboard, down, down}},
            Listener ! {input_event, self(), Timestamp, {keyboard, up, down}}
    end,
    ok.
