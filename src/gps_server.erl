-module(gps_server).
-export([start/1]).

start(Fun) ->
    {ok,
        spawn(fun() ->
            UART = uart:open("UART1", [{tx_pin, 43}, {rx_pin, 44}, {speed, 38400}]),
            read_uart(UART, Fun, <<>>, [])
        end)}.

read_uart(U, Fun, Acc, Tokens) ->
    {ok, R} = uart:read(U),

    In = <<Acc/binary, R/binary>>,
    try nmea_parser:tokenize_next(In, Tokens) of
        {cont, Rest, NewTokens} ->
            read_uart(U, Fun, Rest, NewTokens);
        {ok, NewTokens, Rest} ->
            try nmea_parser:parse(NewTokens) of
                {ok, Valid} ->
                    Fun(Valid),
                    read_uart(U, Fun, <<"">>, []);
                Any ->
                    erlang:display(Any),
                    read_uart(U, Fun, Rest, [])
            catch
                _:_ ->
                    erlang:display({failed_parse, NewTokens}),
                    read_uart(U, Fun, <<"">>, [])
            end;
        Unexpected ->
            erlang:display({unexpected, Unexpected}),
            read_uart(U, Fun, <<"">>, [])
    catch
        _:_ ->
            erlang:display(failed_tokenize),
            read_uart(U, Fun, <<"">>, [])
    end.
