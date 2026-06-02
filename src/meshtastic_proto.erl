-module(meshtastic_proto).
-export([decode/1, encode/1]).

-define(MAIN_SCHEMA, #{
    portnum => {1, ?PORTNUM_ENUM},
    payload => {2, bytes},
    want_response => {3, bool},
    dest => {4, fixed32},
    source => {5, fixed32},
    request_id => {6, fixed32},
    reply_id => {7, fixed32},
    emoji => {8, fixed32},
    bitfield => {9, uint32}
}).
-define(PORTNUM_ENUM,
    {enum, #{
        'TEXT_MESSAGE_APP' => 1,
        'POSITION_APP' => 3,
        'NODEINFO_APP' => 4,
        'ROUTING_APP' => 5,
        'TELEMETRY_APP' => 67,
        'TRACEROUTE_APP' => 70
    }}
).
-define(ROUTING_SCHEMA, #{
    error_reason => {3, {enum, #{'NONE' => 0}}}
}).
-define(ROUTE_DISCOVERY_SCHEMA, #{
    route => {1, {repeated, fixed32}},
    snr_towards => {2, {repeated, int32}},
    route_back => {3, {repeated, fixed32}},
    snr_back => {4, {repeated, int32}}
}).
-define(POSITION_SCHEMA, #{
    time => {4, sfixed32},
    latitude_i => {1, sfixed32},
    longitude_i => {2, sfixed32},
    altitude => {3, int32}
}).
-define(USER_SCHEMA, #{
    id => {1, string},
    long_name => {2, string},
    short_name => {3, string},
    macaddr => {4, bytes},
    hw_model => {5, int32},
    is_licensed => {6, bool},
    role => {7, ?ROLE},
    public_key => {8, bytes},
    is_unmessagable => {9, bool}
}).
-define(ROLE,
    {enum, #{
        'CLIENT' => 0,
        'CLIENT_MUTE' => 1,
        'ROUTER' => 2,
        'ROUTER_CLIENT' => 3,
        'REPEATER' => 4,
        'TRACKER' => 5,
        'SENSOR' => 6,
        'TAK' => 7,
        'CLIENT_HIDDEN' => 8,
        'LOST_AND_FOUND' => 9,
        'TAK_TRACKER' => 10,
        'ROUTER_LATE' => 11,
        'CLIENT_BASE' => 12
    }}
).
-define(TELEMETRY_SCHEMA, #{
    time => {1, fixed32},
    device_metrics => {2, bytes}
}).
-define(DEVICE_METRICS_SCHEMA, #{
    battery_level => {1, uint32},
    voltage => {2, float},
    channel_utilization => {3, float},
    air_util_tx => {4, float}
}).

decode(Data) ->
    MainSchema = aprotobuf_decoder:transform_schema(?MAIN_SCHEMA),
    ParsedMain = aprotobuf_decoder:parse(Data, MainSchema),
    case ParsedMain of
        #{portnum := 'TEXT_MESSAGE_APP'} ->
            ParsedMain;
        #{portnum := 'POSITION_APP', payload := Payload} ->
            PositionSchema = aprotobuf_decoder:transform_schema(?POSITION_SCHEMA),
            NewPayload = aprotobuf_decoder:parse(Payload, PositionSchema),
            ParsedMain#{payload := NewPayload};
        #{portnum := 'NODEINFO_APP', payload := Payload} ->
            UserSchema = aprotobuf_decoder:transform_schema(?USER_SCHEMA),
            NewPayload = aprotobuf_decoder:parse(Payload, UserSchema),
            ParsedMain#{payload := NewPayload};
        #{portnum := 'TELEMETRY_APP', payload := Payload} ->
            TelemetrySchema = aprotobuf_decoder:transform_schema(?TELEMETRY_SCHEMA),
            Telemetry = aprotobuf_decoder:parse(Payload, TelemetrySchema),
            NewPayload =
                case Telemetry of
                    #{device_metrics := DmBytes} ->
                        DmSchema = aprotobuf_decoder:transform_schema(?DEVICE_METRICS_SCHEMA),
                        Telemetry#{device_metrics := aprotobuf_decoder:parse(DmBytes, DmSchema)};
                    _ ->
                        Telemetry
                end,
            ParsedMain#{payload := NewPayload};
        #{portnum := 'TRACEROUTE_APP'} ->
            RouteSchema = aprotobuf_decoder:transform_schema(?ROUTE_DISCOVERY_SCHEMA),
            Payload = maps:get(payload, ParsedMain, <<>>),
            ParsedMain#{payload => aprotobuf_decoder:parse(Payload, RouteSchema)};
        Any ->
            Any
    end.

encode(Map) ->
    case Map of
        #{portnum := 'TEXT_MESSAGE_APP'} ->
            aprotobuf_encoder:encode(Map, ?MAIN_SCHEMA);
        #{portnum := 'POSITION_APP', payload := PayloadMap} ->
            Payload = aprotobuf_encoder:encode(PayloadMap, ?POSITION_SCHEMA),
            NewMap = Map#{payload := Payload},
            aprotobuf_encoder:encode(NewMap, ?MAIN_SCHEMA);
        #{portnum := 'NODEINFO_APP', payload := PayloadMap} ->
            Payload = aprotobuf_encoder:encode(PayloadMap, ?USER_SCHEMA),
            NewMap = Map#{payload := Payload},
            aprotobuf_encoder:encode(NewMap, ?MAIN_SCHEMA);
        #{portnum := 'ROUTING_APP', payload := PayloadMap} ->
            Payload = aprotobuf_encoder:encode(PayloadMap, ?ROUTING_SCHEMA),
            NewMap = Map#{payload := Payload},
            aprotobuf_encoder:encode(NewMap, ?MAIN_SCHEMA);
        #{portnum := 'TRACEROUTE_APP', payload := PayloadMap} ->
            Payload = aprotobuf_encoder:encode(PayloadMap, ?ROUTE_DISCOVERY_SCHEMA),
            NewMap = Map#{payload := Payload},
            aprotobuf_encoder:encode(NewMap, ?MAIN_SCHEMA)
    end.
