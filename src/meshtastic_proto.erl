-module(meshtastic_proto).
-export([decode/1, encode/1]).

-define(MAIN_SCHEMA, #{
    portnum => {1, ?PORTNUM_ENUM}, payload => {2, bytes}, want_response => {3, bool}
}).
-define(PORTNUM_ENUM,
    {enum, #{
        'TEXT_MESSAGE_APP' => 1, 'POSITION_APP' => 3, 'NODEINFO_APP' => 4, 'TELEMETRY_APP' => 67
    }}
).
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
    is_licensed => {6, bool}
}).
-define(TELEMETRY_SCHEMA, #{
    time => {1, fixed32},
    device_metrics => {2, bytes}
}).
%-define(DEVICE_METRICS_SCHEMA, #{
%    battery_level => {1, int32}, %uint32
%    voltage => {2, float},
%    channel_utilization => {3, float},
%    air_util_tx => {4, float}
%}).

decode(Data) ->
    MainSchema = uprotobuf_decoder:transform_schema(?MAIN_SCHEMA),
    ParsedMain = uprotobuf_decoder:parse(Data, MainSchema),
    case ParsedMain of
        #{portnum := 'TEXT_MESSAGE_APP'} ->
            ParsedMain;
        #{portnum := 'POSITION_APP', payload := Payload} ->
            PositionSchema = uprotobuf_decoder:transform_schema(?POSITION_SCHEMA),
            NewPayload = uprotobuf_decoder:parse(Payload, PositionSchema),
            ParsedMain#{payload := NewPayload};
        #{portnum := 'NODEINFO_APP', payload := Payload} ->
            UserSchema = uprotobuf_decoder:transform_schema(?USER_SCHEMA),
            NewPayload = uprotobuf_decoder:parse(Payload, UserSchema),
            ParsedMain#{payload := NewPayload};
        #{portnum := 'TELEMETRY_APP', payload := Payload} ->
            TelemetrySchema = uprotobuf_decoder:transform_schema(?TELEMETRY_SCHEMA),
            NewPayload = uprotobuf_decoder:parse(Payload, TelemetrySchema),
            ParsedMain#{payload := NewPayload};
        Any ->
            Any
    end.

encode(Map) ->
    case Map of
        #{portnum := 'TEXT_MESSAGE_APP'} ->
            uprotobuf_encoder:encode(Map, ?MAIN_SCHEMA);
        #{portnum := 'POSITION_APP', payload := PayloadMap} ->
            Payload = uprotobuf_encoder:encode(PayloadMap, ?POSITION_SCHEMA),
            NewMap = Map#{payload := Payload},
            uprotobuf_encoder:encode(NewMap, ?MAIN_SCHEMA)
    end.
