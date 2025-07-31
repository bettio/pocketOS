-module(bq25896_driver).

-behavior(gen_server).

-export([
    start_link/0,
    open/1,
    shutdown/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {i2c}).

-define(ADDRESS, 16#6B).
-define(POWERS_PPM_REG_09H, 16#09).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

open(Pid) ->
    gen_server:call(Pid, open).

shutdown(Pid) ->
    gen_server:call(Pid, shutdown).

init([]) ->
    erlang:display("Init bq25896_driver gen_server"),
    {ok, #state{}}.

handle_call(open, _From, _State) ->
    I2C =
        case whereis(main_i2c) of
            undefined ->
                OpenI2C = i2c:open([{scl_io_num, 2}, {sda_io_num, 3}, {i2c_clock_hz, 100000}]),
                register(main_i2c, OpenI2C),
                OpenI2C;
            OpenI2C ->
                OpenI2C
        end,
    {reply, ok, #state{i2c = I2C}};
handle_call(shutdown, {_Pid, _Ref}, #state{i2c = I2C} = State) ->
    set_register_bit(I2C, ?POWERS_PPM_REG_09H, 5),
    {reply, ok, State};
handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.

read_register(I2C, Reg) ->
    ok = i2c:write_bytes(I2C, ?ADDRESS, Reg),
    {ok, <<Byte>>} = i2c:read_bytes(I2C, ?ADDRESS, 1),
    {ok, Byte}.

write_register(I2C, Reg, Value) ->
    ok = i2c:write_bytes(I2C, ?ADDRESS, <<Reg:8, Value:8>>),
    ok.

set_register_bit(I2C, Reg, BitIndex) ->
    {ok, RegValue} = read_register(I2C, Reg),
    NewRegValue = RegValue bor (1 bsl BitIndex),
    ok = write_register(I2C, Reg, NewRegValue).
