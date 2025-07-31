-module(tca8418_driver).

-behavior(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {listener, i2c}).

-define(GPIO_NUM, 6).
-define(ADDRESS, 16#34).

-define(REG_CFG, 16#01).
-define(REG_INT_STAT, 16#02).
-define(REG_KEY_EVENT_A, 16#04).
-define(REG_GPI_EM_1, 16#20).
-define(REG_GPI_EM_2, 16#21).
-define(REG_GPI_EM_3, 16#22).
-define(REG_GPIO_DIR_1, 16#23).
-define(REG_GPIO_DIR_2, 16#24).
-define(REG_GPIO_DIR_3, 16#25).
-define(REG_GPIO_INT_LVL_1, 16#26).
-define(REG_GPIO_INT_LVL_2, 16#27).
-define(REG_GPIO_INT_LVL_3, 16#28).
-define(REG_GPIO_INT_EN_1, 16#1A).
-define(REG_GPIO_INT_EN_2, 16#1B).
-define(REG_GPIO_INT_EN_3, 16#1C).
-define(REG_KP_GPIO_1, 16#1D).
-define(REG_KP_GPIO_2, 16#1E).
-define(REG_KP_GPIO_3, 16#1F).

-define(REG_CFG_GPI_IEN, 16#02).
-define(REG_CFG_KE_IEN, 16#01).

-define(KEYMAP, {
    {$q, $w, $e, $r, $t, $y, $u, $i, $o, $p},
    {$a, $s, $d, $f, $g, $h, $j, $k, $l, $\n},
    {0, $z, $x, $c, $v, $b, $n, $m, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    erlang:display("Init tca8418 gen_server"),
    {ok, #state{}}.

handle_call(open, _From, _State) ->
    GPIO = gpio:start(),

    gpio:set_pin_pull(?GPIO_NUM, up),
    gpio:set_pin_mode(?GPIO_NUM, input),

    %TODO: change this
    gpio:set_int(GPIO, ?GPIO_NUM, falling),

    I2C =
        case whereis(main_i2c) of
            undefined ->
                OpenI2C = i2c:open([{scl_io_num, 2}, {sda_io_num, 3}, {i2c_clock_hz, 100000}]),
                register(main_i2c, OpenI2C),
                OpenI2C;
            OpenI2C ->
                OpenI2C
        end,

    init_default(I2C),

    {reply, ok, #state{i2c = I2C}};
handle_call({subscribe_input, all}, {Pid, _Ref}, State) ->
    {reply, ok, State#state{listener = Pid}};
handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info({gpio_interrupt, ?GPIO_NUM}, #state{listener = Listener, i2c = I2C} = State) ->
    case read_register(I2C, ?REG_KEY_EVENT_A) of
        {ok, Num} ->
            write_register(I2C, ?REG_INT_STAT, 1),
            Evt = keyevent_to_event(Num),
            Listener ! {input_event, self(), erlang:system_time(millisecond), Evt};
        NotOk ->
            erlang:display({not_ok, NotOk})
    end,
    {noreply, State};
handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.

keyevent_to_event(KeyEvent) ->
    UpDown =
        case (KeyEvent band 16#80) of
            0 -> down;
            16#80 -> up
        end,
    MaskedCode = (KeyEvent band 16#7F) - 1,
    Row = MaskedCode div 10,
    Column = MaskedCode rem 10,
    RowTuple = element(Row + 1, ?KEYMAP),
    Char = element(Column + 1, RowTuple),
    {keyboard, UpDown, Char}.

init_default(I2C) ->
    write_register(I2C, ?REG_GPIO_DIR_1, 16#00),
    write_register(I2C, ?REG_GPIO_DIR_2, 16#00),
    write_register(I2C, ?REG_GPIO_DIR_3, 16#00),

    write_register(I2C, ?REG_GPI_EM_1, 16#FF),
    write_register(I2C, ?REG_GPI_EM_2, 16#FF),
    write_register(I2C, ?REG_GPI_EM_3, 16#FF),

    write_register(I2C, ?REG_GPIO_INT_LVL_1, 16#00),
    write_register(I2C, ?REG_GPIO_INT_LVL_2, 16#00),
    write_register(I2C, ?REG_GPIO_INT_LVL_3, 16#00),

    write_register(I2C, ?REG_GPIO_INT_EN_1, 16#FF),
    write_register(I2C, ?REG_GPIO_INT_EN_2, 16#FF),
    write_register(I2C, ?REG_GPIO_INT_EN_3, 16#FF),

    %4
    Rows = 2#1111,
    % Cols are 10

    % 8
    Cols_1 = 2#11111111,
    % 2
    Cols_2 = 2#11,

    write_register(I2C, ?REG_KP_GPIO_1, Rows),
    write_register(I2C, ?REG_KP_GPIO_2, Cols_1),
    write_register(I2C, ?REG_KP_GPIO_3, Cols_2),

    % flush
    read_until_zero(I2C, ?REG_KEY_EVENT_A),
    %% reset counters
    write_register(I2C, ?REG_INT_STAT, 3),

    % enable interrupts
    {ok, Value} = read_register(I2C, ?REG_CFG),
    NewValue = Value bor (?REG_CFG_GPI_IEN bor ?REG_CFG_KE_IEN),
    write_register(I2C, ?REG_CFG, NewValue),

    ok.

read_register(I2C, Reg) ->
    ok = i2c:write_bytes(I2C, ?ADDRESS, Reg),
    {ok, <<Byte>>} = i2c:read_bytes(I2C, ?ADDRESS, 1),
    {ok, Byte}.

write_register(I2C, Reg, Value) ->
    ok = i2c:write_bytes(I2C, ?ADDRESS, <<Reg:8, Value:8>>),
    ok.

read_until_zero(I2C, Reg) ->
    case read_register(I2C, Reg) of
        {ok, 0} -> ok;
        {ok, _} -> read_until_zero(I2C, Reg)
    end.
