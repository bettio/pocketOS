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

-record(state, {listener, i2c, modifier = none}).

-define(GPIO_NUM, 6).
-define(ADDRESS, 16#34).

-define(REG_CFG, 16#01).
-define(REG_INT_STAT, 16#02).
-define(REG_LCK_EC, 16#03).
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

-define(KEYMAP,
    {
        {$q, $w, $e, $r, $t, $y, $u, $i, $o, $p},
        {$a, $s, $d, $f, $g, $h, $j, $k, $l, $\n},
        {{switch, symbol}, $z, $x, $c, $v, $b, $n, $m, {switch, shift}, $\b},
        {$\s, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    }
).

-define(SHIFTMAP,
    {
        {$Q, $W, $E, $R, $T, $Y, $U, $I, $O, $P},
        {$A, $S, $D, $F, $G, $H, $J, $K, $L, $\n},
        {0, $Z, $X, $C, $V, $B, $N, $M, 0, $\b},
        {$\s, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    }
).

-define(CAPSMAP,
    {
        {$Q, $W, $E, $R, $T, $Y, $U, $I, $O, $P},
        {$A, $S, $D, $F, $G, $H, $J, $K, $L, $\n},
        {{switch, none}, $Z, $X, $C, $V, $B, $N, $M, {switch, none}, $\b},
        {$\s, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    }
).

-define(SYMMAP,
    {
        {$1, $2, $3, $4, $5, $6, $7, $8, $9, $0},
        {$*, $/, $+, $-, $=, $:, $', $", $@, $\n},
        {{switch, shift_symbol}, $_, $$, $;, $?, $!, $,, $., {switch, capslock}, $\b},
        {$\s, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    }
).

-define(SHIFTSYMMAP,
    {
        {$<, $>, $[, $], $(, $), ${, $}, $|, 0},
        {$&, $%, $^, $#, $\\, $~, 0, 0, 0, 0},
        {{switch, symbol}, 0, 0, 0, 0, 0, 0, 0, {switch, none}, $\b},
        {$\s, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    }
).

-define(ROW_COUNT, 4).
-define(COL_COUNT, 10).

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

handle_info(
    {gpio_interrupt, ?GPIO_NUM}, #state{listener = Listener, i2c = I2C, modifier = Modifier} = State
) ->
    NewModifier =
        case read_register(I2C, ?REG_INT_STAT) of
            % K_INT
            {ok, 0} -> read_and_send_events(I2C, Listener, Modifier);
            % GPI_INT
            {ok, 1} -> read_and_send_events(I2C, Listener, Modifier)
        end,
    {noreply, State#state{modifier = NewModifier}};
handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.

read_and_send_events(I2C, Listener, Modifier) ->
    case read_register(I2C, ?REG_KEY_EVENT_A) of
        {ok, 0} ->
            clear_interrupt(I2C),
            Modifier;
        {ok, Num} ->
            NewModifier =
                case keyevent_to_event(Num, Modifier) of
                    {{keyboard, _UpDown, _Char} = Evt, ReturnedModifier} ->
                        erlang:display(
                            {going_to_send, Listener,
                                {input_event, self(), erlang:system_time(millisecond), Evt}}
                        ),
                        Listener ! {input_event, self(), erlang:system_time(millisecond), Evt},
                        ReturnedModifier;
                    {{ignored, modifier}, ReturnedModifier} ->
                        ReturnedModifier;
                    {{ignored, Code}, ReturnedModifier} ->
                        erlang:display({tca8418, ignored, Code}),
                        ReturnedModifier
                end,
            {ok, LkcEcValue} = read_register(I2C, ?REG_LCK_EC),
            PendingCount = LkcEcValue band 16#F,
            case PendingCount of
                0 -> clear_interrupt(I2C);
                _N -> read_and_send_events(I2C, Listener, NewModifier)
            end,
            NewModifier
    end.

clear_interrupt(I2C) ->
    write_register(I2C, ?REG_INT_STAT, 1).

keyevent_to_event(KeyEvent, Modifier) ->
    UpDown =
        case (KeyEvent band 16#80) of
            0 -> up;
            16#80 -> down
        end,
    MaskedCode = (KeyEvent band 16#7F) - 1,
    Row = MaskedCode div 10,
    Column = MaskedCode rem 10,
    if
        Row >= ?ROW_COUNT -> {{ignored, KeyEvent}, Modifier};
        Column >= ?COL_COUNT -> {{ignored, KeyEvent}, Modifier};
        true -> row_col_to_char(Row, Column, UpDown, Modifier)
    end.

row_col_to_char(Row, Column, UpDown, Modifier) ->
    RowTuple = element(Row + 1, modifier_to_layer(Modifier)),
    case element(Column + 1, RowTuple) of
        0 ->
            {{ignored, {Row, Column, UpDown}}, next_modifier(Modifier)};
        {switch, NewModifier} when UpDown == up ->
            {{ignored, NewModifier}, NewModifier};
        Char ->
            {{keyboard, UpDown, Char}, next_modifier(Modifier)}
    end.

modifier_to_layer(Modifier) ->
    case Modifier of
        none -> ?KEYMAP;
        shift -> ?SHIFTMAP;
        capslock -> ?CAPSMAP;
        symbol -> ?SYMMAP;
        shift_symbol -> ?SHIFTSYMMAP
    end.

next_modifier(Modifier) ->
    case Modifier of
        shift -> none;
        shift_symbol -> none;
        Other -> Other
    end.

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
