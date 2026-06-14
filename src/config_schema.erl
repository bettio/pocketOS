-module(config_schema).

%% Pure validation/coercion engine for the layered config loader. A schema is a
%% map of Key => entry; an entry declares a type plus optional default/values/nvs.
%% Raw values arrive either sxp-native (integer/atom/binary) or as an NVS binary.

-export([validate/2, coerce/2]).

-type entry() :: #{
    type := int | bool | string | enum,
    default => term(),
    values => [atom()],
    nvs => atom() | binary()
}.
-type schema() :: #{atom() => entry()}.

-export_type([entry/0, schema/0]).

%% @doc Coerce a single raw value to its entry's type.
-spec validate(entry(), term()) -> {ok, term()} | {error, term()}.
validate(#{type := int}, V) when is_integer(V) ->
    {ok, V};
validate(#{type := int}, V) when is_binary(V) ->
    try
        {ok, binary_to_integer(V)}
    catch
        error:badarg -> {error, not_an_integer}
    end;
validate(#{type := bool}, V) when is_boolean(V) ->
    {ok, V};
validate(#{type := bool}, <<"true">>) ->
    {ok, true};
validate(#{type := bool}, <<"false">>) ->
    {ok, false};
validate(#{type := bool}, <<"1">>) ->
    {ok, true};
validate(#{type := bool}, <<"0">>) ->
    {ok, false};
validate(#{type := string}, V) when is_binary(V) ->
    {ok, V};
validate(#{type := string}, V) when is_atom(V) ->
    {ok, atom_to_binary(V, utf8)};
validate(#{type := enum, values := Vals}, V) when is_atom(V) ->
    case lists:member(V, Vals) of
        true -> {ok, V};
        false -> {error, {not_in_enum, V}}
    end;
validate(#{type := enum} = E, V) when is_binary(V) ->
    try
        validate(E, binary_to_existing_atom(V, utf8))
    catch
        error:badarg -> {error, {not_in_enum, V}}
    end;
validate(_Entry, _V) ->
    {error, bad_value}.

%% @doc Coerce a raw key/value map against a schema. Iterates schema keys, so
%% unknown keys are reported (not propagated); an absent or invalid value falls
%% back to the entry default (omitted when there is none).
-spec coerce(schema(), map()) -> {map(), [{atom(), term()}]}.
coerce(Schema, Raw) ->
    {Coerced, Errs} =
        maps:fold(
            fun(Key, Entry, {Acc, Es}) -> coerce_key(Key, Entry, Raw, Acc, Es) end,
            {#{}, []},
            Schema
        ),
    Unknown = [{K, unknown_key} || K <- maps:keys(Raw), not maps:is_key(K, Schema)],
    {Coerced, Errs ++ Unknown}.

coerce_key(Key, Entry, Raw, Acc, Errs) ->
    case maps:find(Key, Raw) of
        {ok, RawVal} ->
            case validate(Entry, RawVal) of
                {ok, Val} -> {Acc#{Key => Val}, Errs};
                {error, R} -> {with_default(Key, Entry, Acc), [{Key, R} | Errs]}
            end;
        error ->
            {with_default(Key, Entry, Acc), Errs}
    end.

with_default(Key, #{default := D} = E, Acc) ->
    case validate(E, D) of
        {ok, V} -> Acc#{Key => V};
        {error, _} -> Acc#{Key => D}
    end;
with_default(_Key, _Entry, Acc) ->
    Acc.
