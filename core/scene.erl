-module(scene).
-export([start/0, draw/2, render_options/2, render_message/3]).

start() ->
    {ok, Display} = display:start(),
    register(display, Display),
    {ok, Input} = input:start(),

    MainMenu = 'Elixir.MainMenu':start(),

    InitState = 'Elixir.IconsMenu':init(),
    Scene = 'Elixir.IconsMenu':render(InitState),

    %InitState = 'Elixir.InputBox':init(),
    %Scene = 'Elixir.InputBox':render(InitState),

    %{noreply_render, NewState, NewScene} = 'Elixir.InputBox':handle({keyboard_event,99,true,758}, InitState),
    %{noreply_render, NewState, NewScene} = 'Elixir.IconsMenu':handle({keyboard_event,19,true,1991}, InitState),

    draw(Display, Scene),

    UART = uart:open("UART0", []),
     loop(UART, Display, InitState).

render_message(info, Title, Text) ->
    render_message(icons64:info_icon(), Title, Text);

render_message(critical, Title, Text) ->
    render_message(icons64:critical_icon(), Title, Text);

render_message(warning, Title, Text) ->
    render_message(icons64:warning_icon(), Title, Text);

render_message(Icon, Title, Text) ->
    [
        {clear_screen, 16#CE59},
        {rect, 0, 0, 320, 18, 16#0010},
        {text, 1, 1, Title, 16#FFFF},
        {image, 1, 80, Icon, 16#CE59},
        {text, 70, 103, Text, 16#00}
    ].

render_options(Options, Selected) ->
    render_options(Options, Selected, 0).

render_options([], _Selected, _Index) ->
    [];

render_options([Text | Options], Selected, Index) when Selected == Index ->
    [
        {rect, 0, 18 * Index, 320, 18, 16#0010},
        {text, 1, 18 * Index + 1, Text, 16#FFFF} |
        render_options(Options, Selected, Index + 1)
    ];

render_options([Text | Options], Selected, Index) ->
    BackColor =
        case Index rem 2 of
            0 -> 16#E73C;
            1 -> 16#D69A
        end,
    [
        {rect, 0, 18 * Index, 320, 18, BackColor},
        {text, 1, 18 * Index + 1, Text, 16#0000} |
        render_options(Options, Selected, Index + 1)
    ].

loop(UART, Display, State) ->
    {ok, R} = uart:read(UART),
    erlang:display({recvbin, R}),
    Msg =
    case erlang:binary_to_list(R) of
            % Serial / simulation
            "s" -> {button, start, press};
            "e" -> {button, select, press};
            "a" -> {button, a, press};
            "b" -> {button, b, press};
            [27, 91, 68] -> {keyboard_event,20,true,1991}; %{button, left, press};
            [27, 91, 65] -> {keyboard_event,17,true,1991}; %{button, up, press};
            [27, 91, 67] -> {keyboard_event,19,true,1991}; %{button, right, press};
            [27, 91, 66] -> {keyboard_event,18,true,1991}; %{button, down, press};

            [13] -> {keyboard_event,13,true,1991};

            % Real hardware
            [16#FC, 16#00, 16#02, 16#00, 16#FE] -> {button, down, press};
            Any -> {none}
        end,
    erlang:display(Msg),

    erlang:display(ready),
    {noreply_render, NewScene, NewState} = 'Elixir.IconsMenu':handle(Msg, State),
    erlang:display(go),

    draw(Display, NewScene),

    loop(UART, Display, NewState).

draw(_Display, []) ->
    ok;

draw(Display, [{clear_screen, Color} | Tail]) ->
    display:clear_screen(Display, Color),
    draw(Display, Tail);

draw(Display, [{image, X, Y, Image, BackgroundColor} | Tail]) ->
    display:draw_image(Display, X, Y, Image, BackgroundColor),
    draw(Display, Tail);

draw(Display, [{rect, X, Y, W, H, Color} | Tail]) ->
    display:draw_rect(Display, X, Y, W, H, Color),
    draw(Display, Tail);

draw(Display, [{text, X, Y, Text, Color} | Tail]) ->
    display:draw_text(Display, X, Y, Text, Color),
    draw(Display, Tail).
