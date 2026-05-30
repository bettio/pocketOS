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
    rolling_packet_id,
    node_id,
    last_seen = #{},
    node_info = #{},
    channel = #{},
    private_key = undefined
}).

-ifdef(MESH_TRACE_ENABLED).
-define(MESH_TRACE(Format, Args), io:format(Format, Args)).
-else.
-define(MESH_TRACE(Format, Args), ok).
-endif.

-define(PACKET_SEEN_EXPIRY_SEC, 30).
%% Mirror Meshtastic Router::generatePacketId: bottom 10 bits = rolling counter,
%% top 22 bits = fresh random per packet. Initial rolling value is random at boot.
-define(PACKET_ID_COUNTER_MASK, 16#3FF).

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
    NodeInfo = proplists:get_value(node_info, MeshtasticOpts),
    Channel = init_channel(proplists:get_value(channel, MeshtasticOpts)),
    PrivateKey = proplists:get_value(private_key, MeshtasticOpts),
    <<InitialRolling:32>> = crypto:strong_rand_bytes(4),
    ?MESH_TRACE("[mesh] init node_id=~p initial_rolling=~p~n", [NodeId, InitialRolling]),

    erlang:send_after(500, self(), periodic),

    {ok, #state{
        radio = Radio,
        callbacks = Callbacks,
        node_id = NodeId,
        rolling_packet_id = InitialRolling,
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
        {ok, #{src := ThisNodeAddress} = _Packet} ->
            ?MESH_TRACE("Discarding packet from this node: ~p.~n", [_Packet]),
            {reply, discard, State};
        {ok,
            #{
                hop_limit := _HopLimit,
                src := Src,
                packet_id := PacketId,
                dest := _Dest,
                channel_hash := _ChannelHash,
                encrypted_data := _EncData
            } = Packet} ->
            ?MESH_TRACE(
                "[mesh] rx src=~p pid=~p [low10=~p hi22=~p] dest=~p ch=~p hop=~p enc_bytes=~p~n",
                [
                    Src,
                    PacketId,
                    PacketId band 16#3FF,
                    PacketId bsr 10,
                    _Dest,
                    _ChannelHash,
                    _HopLimit,
                    byte_size(_EncData)
                ]
            ),
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
                                    WantResponse = maps:get(want_response, Message, false),
                                    DecodedPacket = DecryptedPacket#{
                                        message => Message,
                                        want_response => WantResponse
                                    },
                                    maybe_callback(
                                        State#state.callbacks, message_cb, DecodedPacket
                                    ),
                                    BaseState = State#state{last_seen = PrunedLastSeen},
                                    StateAfterReplies = maybe_send_replies(
                                        Packet, Message, Src, BaseState
                                    ),
                                    RebroadcastState = maybe_rebroadcast(
                                        Packet, StateAfterReplies
                                    ),
                                    {reply, ok, RebroadcastState}
                            catch
                                _:_ ->
                                    ?MESH_TRACE("Failed protobuf decode: ~p.~n", [Decrypted]),
                                    {reply, discard, State}
                            end;
                        {error, _Reason} ->
                            % We don't update last seen when we receive a corrupt packet
                            % just in case next retransmision is ok
                            ?MESH_TRACE("Failed meshtastic decrypt: ~p.~n", [_Reason]),
                            {reply, discard, State}
                    end;
                not Duplicated ->
                    ?MESH_TRACE(
                        "[mesh] rx not-for-us, will forward: src=~p pid=~p~n",
                        [Src, PacketId]
                    ),
                    RebroadcastState = maybe_rebroadcast(Packet, State#state{
                        last_seen = PrunedLastSeen
                    }),
                    {reply, ok, RebroadcastState};
                true ->
                    ?MESH_TRACE("[mesh] rx duplicate src=~p pid=~p~n", [Src, PacketId]),
                    {reply, discard, State}
            end;
        _SomethingElse ->
            ?MESH_TRACE("[mesh] rx parse-failed: ~p~n", [_SomethingElse]),
            {reply, next, State}
    end;
handle_call(
    {send, DestAddr, Data},
    _From,
    #state{
        radio = {_RadioId, RadioModule, Radio},
        node_id = NodeId,
        rolling_packet_id = Rolling,
        channel = #{psk := Psk, hash := ChannelHash}
    } = State
) ->
    {PacketId, NextRolling} = generate_packet_id(Rolling),

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
    ?MESH_TRACE(
        "[mesh] tx pid=~p [low10=~p hi22=~p] dest=~p ch=~p data_bytes=~p wire_bytes=~p~n",
        [
            PacketId,
            PacketId band 16#3FF,
            PacketId bsr 10,
            DestAddr,
            ChannelHash,
            byte_size(Data),
            byte_size(RadioPayload)
        ]
    ),
    _BroadcastResult = RadioModule:broadcast(Radio, RadioPayload),
    ?MESH_TRACE("[mesh] tx pid=~p -> ~p~n", [PacketId, _BroadcastResult]),
    {reply, ok, State#state{rolling_packet_id = NextRolling}};
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

generate_packet_id(Rolling) ->
    NextRolling = (Rolling + 1) band ?PACKET_ID_COUNTER_MASK,
    <<TopRand:22, _:10>> = crypto:strong_rand_bytes(4),
    PacketId = NextRolling bor (TopRand bsl 10),
    {PacketId, NextRolling}.

maybe_send_ack(
    #{want_ack := true, dest := Dest, packet_id := OrigPid},
    Message,
    Src,
    #state{node_id = Dest} = State
) when Dest =/= 16#FFFFFFFF ->
    case maps:is_key(request_id, Message) of
        true ->
            State;
        false ->
            AckData = #{
                portnum => 'ROUTING_APP',
                payload => #{error_reason => 'NONE'},
                request_id => OrigPid
            },
            AckBin = erlang:iolist_to_binary(meshtastic_proto:encode(AckData)),
            ?MESH_TRACE("[mesh] ack pid=~p -> src=~p~n", [OrigPid, Src]),
            {reply, ok, NewState} = handle_call({send, Src, AckBin}, undefined, State),
            NewState
    end;
maybe_send_ack(_, _, _, State) ->
    State.

%% NodeInfo response is handled in meshtastic_server by design.
maybe_send_replies(Packet, Message, Src, State) ->
    case maybe_send_node_info_reply(Packet, Message, Src, State) of
        {true, NewState} -> NewState;
        {false, NewState} -> maybe_send_ack(Packet, Message, Src, NewState)
    end.

maybe_send_node_info_reply(
    #{packet_id := OrigPid},
    #{portnum := 'NODEINFO_APP', want_response := true},
    Src,
    #state{node_info = #{user_info := _UserInfo}} = State
) ->
    ?MESH_TRACE("[mesh] node_info reply -> src=~p req=~p~n", [Src, OrigPid]),
    {true, send_node_info(Src, #{request_id => OrigPid}, State)};
maybe_send_node_info_reply(_Packet, _Message, _Src, State) ->
    {false, State}.

is_recipient(#{dest := Dest}, #state{node_id = NodeId} = _State) ->
    case Dest of
        NodeId -> true;
        16#FFFFFFFF -> true;
        _ -> false
    end.

maybe_rebroadcast(#{dest := Dest, src := _Src, packet_id := _Pid}, #state{node_id = Dest} = State) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (unicast-to-us): src=~p pid=~p~n", [_Src, _Pid]),
    State;
maybe_rebroadcast(
    #{hop_limit := 0, src := _Src, packet_id := _Pid} = _Packet, State
) ->
    ?MESH_TRACE("[mesh] rebroadcast skip (hop=0): src=~p pid=~p~n", [_Src, _Pid]),
    State;
maybe_rebroadcast(
    #{hop_limit := HopLimit, src := _Src, packet_id := _Pid} = Packet,
    #state{radio = {_RadioId, RadioModule, Radio}} = State
) ->
    RadioPayload = meshtastic:serialize(Packet#{hop_limit := HopLimit - 1}),
    ?MESH_TRACE(
        "[mesh] rebroadcast src=~p pid=~p hop=~p->~p wire_bytes=~p~n",
        [_Src, _Pid, HopLimit, HopLimit - 1, byte_size(RadioPayload)]
    ),
    _BroadcastResult = RadioModule:broadcast(Radio, RadioPayload),
    ?MESH_TRACE("[mesh] rebroadcast pid=~p -> ~p~n", [_Pid, _BroadcastResult]),
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

do_periodic_broadcast(#state{node_info = #{user_info := _UserInfo}} = State) ->
    ?MESH_TRACE("Broadcasting user info: ~p.~n", [_UserInfo]),
    NewState = send_node_info(16#FFFFFFFF, #{}, State),
    erlang:send_after(60000, self(), periodic),
    NewState;
do_periodic_broadcast(State) ->
    ?MESH_TRACE("Missing user_info, skipping periodic broadcast.~n", []),
    State.

send_node_info(Dest, Extra, #state{node_info = #{user_info := UserInfo}} = State) ->
    Data = maps:merge(#{portnum => 'NODEINFO_APP', payload => UserInfo}, Extra),
    Bin = erlang:iolist_to_binary(meshtastic_proto:encode(Data)),
    {reply, ok, NewState} = handle_call({send, Dest, Bin}, undefined, State),
    NewState;
send_node_info(_Dest, _Extra, State) ->
    ?MESH_TRACE("Missing user_info, skipping node info send.~n", []),
    State.
