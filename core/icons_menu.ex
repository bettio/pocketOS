defmodule IconsMenu do

  @columns 4

  def init() do
    [
      selected_index: 0,
      icons: [
        {"Test", :icons64.idea_icon()},
        {"Foo", :icons64.warning_icon()},
        {"GBEmu", :icons64.critical_icon()},
        {"Info", :icons64.info_icon()},

        {"Aaa", :icons64.idea_icon()},
        {"Bbb", :icons64.warning_icon()},
        {"Ccc", :icons64.critical_icon()},
        {"Ddd", :icons64.info_icon()},
      ]
    ]
  end

  def render(state) do
    index = Keyword.fetch!(state, :selected_index)
    icons = Keyword.fetch!(state, :icons)

    icons_count = Enum.count(icons)

    rendered_icons =
      Enum.reduce(icons, {0, []}, fn {text, icon}, {i, l} ->
        if i < icons_count do
          {i + 1, render_icon(8 + (64 + 8) * rem(i, @columns), 24 + div(i, @columns) * (48 + 64), text, icon, i == index) ++ l}
        else
          {i, l}
        end
      end)
      |> elem(1)

    [
      {:clear_screen, 0xCE59},

      {:rect, 0, 0, 320, 18, 0x0010},
      {:text, 1, 1, "Icons", 0xFFFF},
    ] ++ rendered_icons
  end

  def render_icon(icon_base_x, icon_base_y, icon_text, icon, selected) do
    icon_size = 64

    bg_color =
      if selected do
        0x0010
      else
        0xCE59
      end

    text_color =
      if selected do
        0xFFFF
      else
        0x0000
      end

    [
      {:rect, icon_base_x, icon_base_y + 8 + 16, icon_size, icon_size, bg_color},
      {:image, icon_base_x, icon_base_y, icon, bg_color},
      {:text, div((icon_size - byte_size(icon_text) * 8), 2) + icon_base_x, icon_base_y + icon_size + 8, icon_text, text_color}
    ]
  end

  def handle({:keyboard_event, keycode, true, _ts} = _event, state) when keycode in [17, 18, 19, 20] do
    index_offset =
      case keycode do
        17 -> -@columns # UP
        18 -> @columns # DOWN
        19 -> 1
        20 -> -1
      end

    index = Keyword.fetch!(state, :selected_index)
    icons = Keyword.fetch!(state, :icons)

    next_index = index + index_offset

    new_state =
      if next_index >= 0 and next_index < Enum.count(icons) do
        Keyword.put(state, :selected_index, index + index_offset)
      else
        state
      end

    new_scene = render(new_state)

    {:noreply_render, new_scene, new_state}
  end

  def handle({:keyboard_event, 13, true, _ts} = _event, state) do
    :erlang.display(:icon_clicked)

    index = 0

    {:reply_norender, {:icon_clicked, index}, state}
  end

  def handle(event, state) do
    :erlang.display(event)

    {:noreply_norender, state}
  end
end
