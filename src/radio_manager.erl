-module(radio_manager).

-behavior(gen_server).

-export([
    start_link/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {handlers = []}).

start_link(RadioConfig, ModuleArg) ->
    gen_server:start_link(?MODULE, [RadioConfig, ModuleArg], []).

init([#{radio_module := RadioModule} = RadioConfig, MA]) ->
    RadioId = RadioModule,

    {ok, Radio} = RadioModule:start(RadioConfig#{receive_handler => self()}),
    register_radio(Radio),

    Handlers = lists:filtermap(
        fun({Name, Module, Args}) ->
            case Module:start_link(Name, {RadioId, RadioModule, Radio}, Args) of
                {ok, Server} ->
                    {true, {Module, Server}};
                Error ->
                    io:format("Failed protocol handler init: ~p~n", [Error]),
                    false
            end
        end,
        MA
    ),

    {ok, #state{handlers = Handlers}}.

handle_call(_msg, _from, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(
    {lora_receive, {_RModel, _RadioPid} = Iface, Payload,
        #{snr := _Snr, rssi := _Rssi} = Attributes},
    State
) ->
    dispatch(State#state.handlers, Iface, Payload, Attributes),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p.~n", [Msg]),
    {noreply, State}.

dispatch([], Iface, Payload, Attributes) ->
    io:format("Failed dispatching for: ~p:~p:~p.~n", [Iface, Attributes, Payload]);
dispatch([{HandlerModule, Handler} | Tail], Iface, Payload, Attributes) ->
    case HandlerModule:handle_payload(Handler, Iface, Payload, Attributes) of
        ok -> ok;
        discard -> ok;
        next -> dispatch(Tail, Iface, Payload, Attributes)
    end.

%% Stable name so the light-sleep path can reach the radio.
register_radio(Radio) ->
    case whereis(lora_radio) of
        undefined -> ok;
        _ -> unregister(lora_radio)
    end,
    try
        register(lora_radio, Radio)
    catch
        error:badarg -> ok
    end.
