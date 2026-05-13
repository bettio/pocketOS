-module(meshtastic_server).

-behavior(gen_server).

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

-record(state, {
    radio,
    callbacks,
    last_packet_id,
    node_id,
    last_seen = #{},
    node_info = #{},
    channel = #{},
    private_key = undefined
}).

-define(PACKET_SEEN_EXPIRY_SEC, 30).

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
    NodeId = proplists:get_value(node_id, MeshtasticOpts, 1127302788),
    InitialPacketId = proplists:get_value(initial_packet_id, MeshtasticOpts, 1),
    NodeInfo = proplists:get_value(node_info, MeshtasticOpts),
    Channel = init_channel(proplists:get_value(channel, MeshtasticOpts)),
    PrivateKey = proplists:get_value(private_key, MeshtasticOpts),

    erlang:send_after(500, self(), periodic),

    {ok, #state{
        radio = Radio,
        callbacks = Callbacks,
        node_id = NodeId,
        last_packet_id = InitialPacketId,
        node_info = NodeInfo,
        channel = Channel,
        private_key = PrivateKey
    }}.

init_channel(undefined) ->
    meshtastic:default_long_fast_channel();
init_channel(#{name := Name, psk := Psk} = Channel) ->
    Channel#{hash => meshtastic:channel_hash(Name, Psk)}.

handle_call({handle_payload, {_IfaceId, _Pid}, Payload, _Attributes}, _From, State) ->
    ThisNodeAddress = State#state.node_id,
    case meshtastic:parse(Payload) of
        {ok, #{src := ThisNodeAddress} = Packet} ->
            io:format("Discarding packet from this node: ~p.~n", [Packet]),
            {reply, discard, State};
        {ok, #{hop_limit := _HopLimit, src := Src, packet_id := PacketId} = Packet} ->
            MonotonicTS = erlang:monotonic_time(second),
            {Duplicated, UpdatedLastSeen} = update_last_seen(
                State#state.last_seen, Src, PacketId, MonotonicTS
            ),
            PrunedLastSeen = prune_expired_last_seen(UpdatedLastSeen, MonotonicTS),
            IsRecipient = is_recipient(Packet, State),
            if
                not Duplicated and IsRecipient ->
                    case try_decrypt(Packet, State) of
                        {ok, #{data := Decrypted} = DecryptedPacket} ->
                            try meshtastic_proto:decode(Decrypted) of
                                Message ->
                                    DecodedPacket = DecryptedPacket#{message => Message},
                                    maybe_callback(
                                        State#state.callbacks, message_cb, DecodedPacket
                                    ),
                                    RebroadcastState = maybe_rebroadcast(Packet, State#state{
                                        last_seen = PrunedLastSeen
                                    }),
                                    {reply, ok, RebroadcastState}
                            catch
                                _:_ ->
                                    io:format("Failed protobuf decode: ~p.~n", [Decrypted]),
                                    {reply, discard, State}
                            end;
                        {error, Reason} ->
                            % We don't update last seen when we receive a corrupt packet
                            % just in case next retransmision is ok
                            io:format("Failed meshtastic decrypt: ~p.~n", [Reason]),
                            {reply, discard, State}
                    end;
                not Duplicated ->
                    RebroadcastState = maybe_rebroadcast(Packet, State#state{
                        last_seen = PrunedLastSeen
                    }),
                    {reply, ok, RebroadcastState};
                true ->
                    {reply, discard, State}
            end;
        _SomethingElse ->
            {reply, next, State}
    end;
handle_call(
    {send, DestAddr, Data},
    _From,
    #state{
        radio = {_RadioId, RadioModule, Radio},
        node_id = NodeId,
        last_packet_id = LastPacketId,
        channel = #{psk := Psk, hash := ChannelHash}
    } = State
) ->
    PacketId = LastPacketId + 1,

    Packet = #{
        dest => DestAddr,
        src => NodeId,
        packet_id => PacketId,
        hop_start => 3,
        via_mqtt => false,
        want_ack => false,
        hop_limit => 3,
        channel_hash => ChannelHash,
        data => Data
    },

    Encrypted = meshtastic:encrypt(Packet, Psk),

    RadioPayload = meshtastic:serialize(Encrypted),
    RadioModule:broadcast(Radio, RadioPayload),
    {reply, ok, State#state{last_packet_id = PacketId}};
handle_call(_msg, _from, State) ->
    {reply, error, State}.

handle_cast(_msg, State) ->
    {reply, error, State}.

handle_info(periodic, State) ->
    NewState = do_periodic_broadcast(State),
    {noreply, NewState};
handle_info(_msg, State) ->
    {noreply, State}.

maybe_callback(undefined, _, _) ->
    ok;
maybe_callback(Mod, Callback, Arg) ->
    Mod:Callback(Arg).

%% PKI mode is signaled by channel_hash == 0 on a unicast packet to us.
%% Falls back to channel decrypt for everything else.
try_decrypt(
    #{channel_hash := 0, dest := Dest, src := Src} = Packet,
    #state{node_id = Dest, private_key = OurPriv, callbacks = Cb}
) when OurPriv =/= undefined, Dest =/= 16#FFFFFFFF ->
    case peer_public_key(Src, Cb) of
        {ok, PeerPub} -> meshtastic:decrypt_pki(Packet, OurPriv, PeerPub);
        {error, _} = E -> E
    end;
try_decrypt(Packet, #state{channel = #{psk := Psk}}) ->
    {ok, meshtastic:decrypt(Packet, Psk)}.

peer_public_key(_NodeId, undefined) ->
    {error, no_callbacks};
peer_public_key(NodeId, Cb) ->
    try Cb:peer_public_key(NodeId) of
        {ok, _} = R -> R;
        Other -> {error, {peer_pubkey_lookup, Other}}
    catch
        _:_ -> {error, peer_pubkey_lookup_failed}
    end.

is_recipient(#{dest := Dest}, #state{node_id = NodeId} = _State) ->
    case Dest of
        NodeId -> true;
        16#FFFFFFFF -> true;
        _ -> false
    end.

maybe_rebroadcast(#{dest := Dest}, #state{node_id = Dest} = State) ->
    State;
maybe_rebroadcast(#{hop_limit := 0} = _Packet, State) ->
    State;
maybe_rebroadcast(
    #{hop_limit := HopLimit} = Packet, #state{radio = {_RadioId, RadioModule, Radio}} = State
) ->
    RadioPayload = meshtastic:serialize(Packet#{hop_limit := HopLimit - 1}),
    RadioModule:broadcast(Radio, RadioPayload),
    State.

% returns: {AlreadySeen, UpdatedLastSeenMap}
update_last_seen(LastSeenMap, Source, PacketId, MonotonicSec) ->
    case LastSeenMap of
        #{Source := #{PacketId := LastTimestamp} = SeenPacketMap} ->
            if
                MonotonicSec > LastTimestamp ->
                    {true, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
                true ->
                    {true, LastSeenMap}
            end;
        #{Source := SeenPacketMap} ->
            {false, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
        _ ->
            {false, LastSeenMap#{Source => #{PacketId => MonotonicSec}}}
    end.

prune_expired_last_seen(LastSeenMap, MonotonicSec) ->
    PrunedMap =
        maps:map(
            fun(_Source, SeenPacketMap) ->
                maps:filter(
                    fun(_PacketId, LastTimestamp) ->
                        if
                            LastTimestamp + ?PACKET_SEEN_EXPIRY_SEC < MonotonicSec ->
                                false;
                            true ->
                                true
                        end
                    end,
                    SeenPacketMap
                )
            end,
            LastSeenMap
        ),
    maps:filter(fun(_Source, SeenPacketMap) -> SeenPacketMap =/= #{} end, PrunedMap).

%%
%% Handle protobuf payloads
%%

do_periodic_broadcast(#state{node_info = #{user_info := UserInfo}} = State) ->
    Data = #{
        portnum => 'NODEINFO_APP',
        payload => UserInfo
    },
    io:format("Broadcasting user info: ~p.~n", [UserInfo]),

    Encoded = meshtastic_proto:encode(Data),
    Bin = erlang:iolist_to_binary(Encoded),

    {reply, ok, NewState} = handle_call({send, 16#FFFFFFFF, Bin}, undefined, State),

    erlang:send_after(60000, self(), periodic),

    NewState;
do_periodic_broadcast(State) ->
    io:format("Missing user_info, skipping periodic broadcast.~n"),
    State.
