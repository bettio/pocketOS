-module(input).
-export([start/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start() ->
    case atomvm:platform() of
        generic_unix ->
            {ok, P} = avm_gen_server:start(?MODULE, [], []),
            Display = whereis(display),
            avm_gen_server:call(Display, {listen, P}, 60000),
            {ok, Display};

        esp32 ->
            % implement here
            {ok, self()}
    end.

init(_) ->
    {ok, nil}.

handle_call(_C, _From, State) ->
    {reply, error, State}.

handle_info({keyboard_event, KeyCode, KeyDown, EventTS} = Event, State) ->
    erlang:display(Event),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

