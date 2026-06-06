-module(meshcore_server).
-behavior(gen_server).

-export([
    start_link/2,
    start_link/3,
    handle_payload/4
]).

%% Pure helpers, exported for tests.
-export([
    enrich/2,
    for_log/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {radio, channel_key}).

start_link(Radio, Opts) ->
    gen_server:start_link(?MODULE, [Radio, Opts], []).

start_link(Name, Radio, Opts) ->
    gen_server:start_link(Name, ?MODULE, [Radio, Opts], []).

handle_payload(Server, {_IfaceId, _Pid} = Iface, Payload, Attributes) ->
    gen_server:call(Server, {handle_payload, Iface, Payload, Attributes}).

init([Radio, Opts]) ->
    ChannelKey = proplists:get_value(
        channel_key, Opts, meshcore_protocol:default_public_channel_key()
    ),
    erlang:display(
        {meshcore_crypto, eddsa_available, meshcore_protocol:eddsa_available(), crypto:info_lib()}
    ),
    {ok, #state{radio = Radio, channel_key = ChannelKey}}.

handle_call(
    {handle_payload, {_IfaceId, _Pid}, Payload, Attributes},
    _From,
    #state{channel_key = ChannelKey} = State
) ->
    case meshcore_protocol:parse(Payload) of
        {ok, Packet} ->
            erlang:display({meshcore_rx, for_log(enrich(Packet, ChannelKey)), Attributes}),
            {reply, ok, State};
        _Invalid ->
            {reply, next, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

enrich(#{type := grp_txt} = Packet, Key) ->
    case meshcore_protocol:decrypt(Packet, Key) of
        {ok, Decrypted} -> Decrypted;
        {error, Reason} -> Packet#{decrypt_error => Reason}
    end;
enrich(#{type := advert} = Packet, _Key) ->
    Packet#{sig_ok => meshcore_protocol:verify_advert(Packet)};
enrich(Packet, _Key) ->
    Packet.

%% Drop the bulky binaries that are just noise in the serial log; the full map
%% from enrich/2 keeps them for storage.
for_log(Packet) ->
    maps:remove(appdata, maps:remove(signature, maps:remove(public_key, Packet))).
