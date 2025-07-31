-module(xl9555_driver).

-behavior(gen_server).

-export([
    start_link/0,
    open/1,
    set_direction/3,
    set_level/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {i2c}).

-define(ADDRESS, 16#20).

-define(CTRL_OUTP0, 16#02).
-define(CTRL_CFG0, 16#06).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

open(Pid) ->
    gen_server:call(Pid, open).

set_direction(Pid, Pin, Dir) ->
    gen_server:call(Pid, {set_direction, Pin, Dir}).

set_level(Pid, Pin, Level) ->
    gen_server:call(Pid, {set_level, Pin, Level}).

init([]) ->
    erlang:display("Init xl9555_driver gen_server"),
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
handle_call({set_direction, Pin, Dir}, {_Pid, _Ref}, #state{i2c = I2C} = State) ->
    i2c_set_direction(I2C, Pin, Dir),
    {reply, ok, State};
handle_call({set_level, Pin, Level}, {_Pid, _Ref}, #state{i2c = I2C} = State) ->
    i2c_set_level(I2C, Pin, Level),
    {reply, ok, State};
handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.

i2c_set_direction(I2C, Pin, Mode) ->
    case Mode of
        output -> clear_register_bit(I2C, ?CTRL_CFG0, Pin);
        input -> set_register_bit(I2C, ?CTRL_CFG0, Pin)
    end.

i2c_set_level(I2C, Pin, Level) ->
    case Level of
        low -> clear_register_bit(I2C, ?CTRL_OUTP0, Pin);
        high -> set_register_bit(I2C, ?CTRL_OUTP0, Pin)
    end.

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

clear_register_bit(I2C, Reg, BitIndex) ->
    {ok, RegValue} = read_register(I2C, Reg),
    NewRegValue = RegValue band (bnot (1 bsl BitIndex)),
    ok = write_register(I2C, Reg, NewRegValue).
