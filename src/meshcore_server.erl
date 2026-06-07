-module(meshcore_server).
-behavior(gen_server).

%%
%% Imperative gen_server layer around the functional core `meshcore_server_core'.
%%
%% Thin shell: it owns the gen_server process and its mailbox, the radio handle,
%% and the clock. Each callback injects the impurity the core needs as data,
%% calls one pure core function, and runs the returned effects. All packet logic
%% lives in the core; see meshcore_server_core.erl.
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

-record(state, {radio, core}).

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
    {Core, Effects} = meshcore_server_core:init(Opts),
    run_effects(Effects),
    {ok, #state{radio = Radio, core = Core}}.

handle_call(
    {handle_payload, {_IfaceId, _Pid}, Payload, Attributes},
    _From,
    #state{core = Core0} = State
) ->
    case meshcore_protocol:parse(Payload) of
        {ok, Packet} ->
            {ok, Core1, Effects} = meshcore_server_core:handle_rx(Packet, Attributes, #{}, Core0),
            run_effects(Effects),
            {reply, ok, State#state{core = Core1}};
        _Invalid ->
            {reply, next, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Effect execution
%%------------------------------------------------------------------------------

run_effects([]) ->
    ok;
run_effects([{deliver, Packet} | Rest]) ->
    erlang:display({meshcore_rx, for_log(Packet)}),
    run_effects(Rest).

%%------------------------------------------------------------------------------
%% Presentation
%%------------------------------------------------------------------------------

%% Drop the bulky binaries that are just noise in the serial log; the full map
%% from the core keeps them for storage.
for_log(Packet) ->
    maps:remove(appdata, maps:remove(signature, maps:remove(public_key, Packet))).
