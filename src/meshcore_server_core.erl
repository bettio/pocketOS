-module(meshcore_server_core).
-moduledoc false.

%%
%% Functional core for `meshcore_server'.
%%
%% Pure: no I/O, no clock, no randomness. The gen_server (`meshcore_server')
%% calls one pure core function per event, runs the returned effects, and holds
%% the state. Everything impure is injected as data through an `Env' map and
%% everything to be done flows back out as an `Effects' list. Mirrors
%% meshtastic_server_core.
%%

-export([
    init/1,
    handle_rx/4,
    enrich/2
]).

-export_type([core_state/0, effect/0, env/0]).

-record(core, {
    channel_key :: binary()
}).

-opaque core_state() :: #core{}.

-type effect() :: {deliver, Packet :: map()}.

-type env() :: #{}.

%%------------------------------------------------------------------------------
%% Construction
%%------------------------------------------------------------------------------

-spec init(proplists:proplist()) -> {core_state(), [effect()]}.
init(Opts) ->
    Core = #core{
        channel_key = proplists:get_value(
            channel_key, Opts, meshcore_protocol:default_public_channel_key()
        )
    },
    {Core, []}.

%%------------------------------------------------------------------------------
%% Receive
%%------------------------------------------------------------------------------

%% Enrich a parsed inbound frame and deliver it with the radio rssi/snr merged
%% in. No forwarding or storage yet.
-spec handle_rx(map(), map(), env(), core_state()) -> {ok, core_state(), [effect()]}.
handle_rx(Packet, Attributes, _Env, #core{channel_key = ChannelKey} = Core) ->
    Delivered = with_attributes(enrich(Packet, ChannelKey), Attributes),
    {ok, Core, [{deliver, Delivered}]}.

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
