-module(scene).
-export([start/0, draw/2, render_options/2, render_message/3]).

start() ->
    {ok, Display} = display:start(),
    register(display, Display),
    {ok, Input} = input:start(),
    InitState = 'Elixir.InputBox':init(),
    Scene = 'Elixir.InputBox':render(InitState),

    KE = [
{keyboard_event,99,true,758},
{keyboard_event,99,false,849},
{keyboard_event,105,true,849},
{keyboard_event,105,false,919},
{keyboard_event,97,true,1009},
{keyboard_event,111,true,1090},
{keyboard_event,97,false,1110},
{keyboard_event,111,false,1150},
{keyboard_event,32,true,1292},
{keyboard_event,32,false,1392},
{keyboard_event,115,true,1483},
{keyboard_event,111,true,1554},
{keyboard_event,115,false,1564},
{keyboard_event,111,false,1626},
{keyboard_event,110,true,1737},
{keyboard_event,110,false,1797},
{keyboard_event,111,true,1888},
{keyboard_event,111,false,1948},
{keyboard_event,32,true,1989},
{keyboard_event,32,false,2119},
{keyboard_event,100,true,2119},
{keyboard_event,100,false,2190},
{keyboard_event,97,true,2301},
{keyboard_event,97,false,2381},
{keyboard_event,118,true,2492},
{keyboard_event,118,false,2573},
{keyboard_event,105,true,2573},
{keyboard_event,105,false,2655},
{keyboard_event,100,true,2675},
{keyboard_event,100,false,2746},
{keyboard_event,101,true,2836},
{keyboard_event,101,false,2907},
{keyboard_event,13,true,4106},
{keyboard_event,13,false,4197}
         ],

    %{noreply_render, NewState, NewScene} = 'Elixir.InputBox':handle({keyboard_event,99,true,758}, InitState),
    
    draw(Display, Scene),
    GBEmu = open_port({spawn, "gbemu"}, []),

    UART = uart:open("UART0", []),
    loop(GBEmu, UART).

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

loop(GBEmu, UART) ->
    {ok, R} = uart:read(UART),
    erlang:display(R),
    Msg =
    case erlang:binary_to_list(R) of
            % Serial / simulation
            "s" -> {button, start, press};
            "e" -> {button, select, press};
            "a" -> {button, a, press};
            "b" -> {button, b, press};
            [27, 91, 68] -> {button, left, press};
            [27, 91, 65] -> {button, up, press};
            [27, 91, 67] -> {button, right, press};
            [27, 91, 66] -> {button, down, press};

            % Real hardware
            [16#FC, 16#00, 16#02, 16#00, 16#FE] -> {button, down, press};
            Any -> {none}
        end,
    erlang:display(ok),
    avm_gen_server:call(GBEmu, Msg, 60000),
    erlang:display(done),
    loop(GBEmu, UART).

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
