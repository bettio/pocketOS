alias PhotonUI.UIServer
alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.ButtonState
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.HorizontalLayout
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.IconListViewState
alias PhotonUI.Widgets.IconGridView
alias PhotonUI.Widgets.IconGridViewState
alias PhotonUI.Widgets.Image
alias PhotonUI.Widgets.ImageState
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.Widgets.TextInputState
alias PhotonUI.Widgets.VerticalLayout

defmodule PhotonUI.Widgets.Button do
  @enforce_keys [:name]
  defstruct [:name, :text, :x, :y, :width, :height]

  def accepts_mouse_events(_widget, _ui_state), do: true
  def can_be_focused?(_widget, _ui_state), do: true
  def state_struct_module(), do: ButtonState

  def render(button, name, ui_state, origin_x, origin_y, acc) do
    %Button{text: text, x: x, y: y, width: width, height: height} = button

    x_pos = origin_x + x
    y_pos = origin_y + y

    # assuming default 16 px
    text_width = byte_size(text) * 8

    button_color =
      case ui_state[name].state do
        :pressed ->
          0x0000FF

        _ ->
          if ui_state[:"$focused_item"] == name do
            0x8080FF
          else
            0xFFFFFF
          end
      end

    [
      {:text, x_pos + 1 + div(width - text_width, 2), y_pos + div(height - 16, 2), :default16px,
       0x000000, button_color, text},
      {:rect, x_pos + 1, y_pos + 1, width - 2, height - 2, button_color},
      {:rect, x_pos, y_pos, width, height, 0x000000}
      | acc
    ]
  end
end

defmodule PhotonUI.Widgets.ButtonState do
  defstruct state: :normal

  def handle_input(button, {:mouse, :pressed, :left, _x, _y}, _ts) do
    %ButtonState{button | state: :pressed}
  end

  def handle_input(button, {:mouse, :released, :left, _x, _y}, _ts) do
    {%ButtonState{button | state: :released}, [event: :clicked]}
  end

  def handle_input(button, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    %ButtonState{button | state: :pressed}
  end

  def handle_input(button, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    {%ButtonState{button | state: :released}, [event: :clicked]}
  end

  def handle_input(button, event, ts) do
    PhotonUI.UIServer.default_input_handler(button, event, ts)
  end
end

defmodule PhotonUI.Widgets.Container do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :children]

  def render(container, _name, ui_state, origin_x, origin_y, acc) do
    %Container{children: children, x: x, y: y} = container
    UIServer.render_widgets(children, ui_state, origin_x + x, origin_y + y, acc)
  end
end

defmodule PhotonUI.Widgets.HorizontalLayout do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :children, spacing: 0]

  def render(h_layout, _name, ui_state, origin_x, origin_y, acc) do
    %HorizontalLayout{children: children, x: x, y: y, spacing: spacing} = h_layout

    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_x}, fn %{x: wx, width: ww} = item, {rendered, x_off} ->
        {UIServer.render_widgets([item], ui_state, x_off + x, origin_y + y, rendered),
         x_off + wx + ww + spacing}
      end)

    acc_with_children
  end

  def prepend_mouse_area(widget, ui_state, origin_x, origin_y, acc) do
    %HorizontalLayout{children: children, x: x, y: y, spacing: spacing} = widget

    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_x}, fn %{x: wx, width: ww} = item,
                                                {mouse_area_acc, x_off} ->
        {UIServer.build_mouse_area_list(
           [item],
           ui_state,
           x_off + x,
           origin_y + y,
           mouse_area_acc
         ), x_off + wx + ww + spacing}
      end)

    acc_with_children
  end
end

defmodule PhotonUI.Widgets.IconListView do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :icon_size, :cell_width, :cell_height]

  def can_be_focused?(_widget, _ui_state), do: true
  def accepts_mouse_events(_widget, _ui_state), do: true
  def state_struct_module(), do: IconListViewState
  def has_init_function(), do: true

  def render(icon_widget, name, ui_state, origin_x, origin_y, acc) do
    %IconListView{
      x: x,
      y: y,
      width: width,
      height: height,
      icon_size: icon_size,
      cell_height: cell_height
    } = icon_widget

    %{
      count: count,
      images: images,
      model: model,
      selected_index: selected_index,
      state: state
    } = ui_state[name]

    rows_per_page = div(height, cell_height)
    page = div(selected_index, rows_per_page)

    # TODO: maybe Enum.reduce_while
    {_, icons} =
      model
      |> Enum.slice(page * rows_per_page, rows_per_page)
      |> Enum.reduce({0, acc}, fn v, {index, grid_acc} ->
        x_pos = origin_x + x
        y_pos = origin_y + y + cell_height * index
        icon_xpos = x_pos + 8
        icon_ypos = y_pos + div(cell_height - icon_size, 2)
        text_xpos = icon_xpos + icon_size + 8
        text_ypos = y_pos + div(cell_height - 16, 2)

        {text_color, bg_color, with_background} =
          if index + page * rows_per_page == selected_index do
            color =
              if state == :released do
                0x0000FF
              else
                0x000080
              end

            {0xFFFFFF, color, [{:rect, x_pos, y_pos, width, cell_height, color} | grid_acc]}
          else
            {0x000000, 0xFFFFFF, grid_acc}
          end

        {index + 1,
         [
           {:text, text_xpos, text_ypos, :default16px, text_color, bg_color, v[:text]},
           {:image, icon_xpos, icon_ypos, bg_color, images[index]}
           | with_background
         ]}
      end)

    pages = div(count, rows_per_page) + 1

    if pages > 1 do
      scroll_height = div(height - 2, pages)
      scroll_yoff = scroll_height * page
      [{:rect, width - 6, origin_y + y + scroll_yoff, 2, scroll_height, 0x000000} | icons]
    else
      icons
    end
  end
end

defmodule PhotonUI.Widgets.IconListViewState do
  defstruct [:images, :model, :count, :selected_index, :state]

  @compile {:no_warn_undefined, :atomvm}

  def init(_widget) do
    %__MODULE__{
      state: :released,
      selected_index: 0,
      images: %{},
      model: [],
      count: 0
    }
  end

  defp load_images(model) do
    model
    |> Enum.reduce({0, []}, fn item, {index, acc} ->
      {index + 1, [{index, ImageState.load_image(item[:source])} | acc]}
    end)
    |> elem(1)
    |> Enum.into(%{})
  end

  def update_property(%__MODULE__{} = s, property, value) do
    case property do
      :model ->
        %__MODULE__{s | model: value, count: Enum.count(value), images: load_images(value)}
    end
  end

  def handle_input(grid_state, {:keyboard, :down, :up}, _ts) do
    %{selected_index: selected_index, count: count} = grid_state

    next_index =
      case rem(selected_index - 1, count) do
        -1 -> count - 1
        pos -> pos
      end

    {%__MODULE__{grid_state | selected_index: next_index},
     [event: {:selected_item_changed, next_index}]}
  end

  def handle_input(grid_state, {:keyboard, :down, :down}, _ts) do
    %{selected_index: selected_index, count: count} = grid_state

    next_index = rem(selected_index + 1, count)

    {%__MODULE__{grid_state | selected_index: next_index},
     [event: {:selected_item_changed, next_index}]}
  end

  def handle_input(grid_state, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    %__MODULE__{grid_state | state: :pressed}
  end

  def handle_input(grid_state, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    %__MODULE__{selected_index: selected_index, model: model} = grid_state

    item = Enum.at(model, selected_index)

    {%__MODULE__{grid_state | state: :released}, [event: {:clicked, selected_index, item}]}
  end

  def handle_input(button, event, ts) do
    PhotonUI.UIServer.default_input_handler(button, event, ts)
  end
end

defmodule PhotonUI.Widgets.IconGridView do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :icon_size, :cell_width, :cell_height]

  def can_be_focused?(_widget, _ui_state), do: true
  def accepts_mouse_events(_widget, _ui_state), do: true
  def state_struct_module(), do: IconGridViewState
  def has_init_function(), do: true

  def render(icon_widget, name, ui_state, origin_x, origin_y, acc) do
    %IconGridView{
      x: x,
      y: y,
      width: width,
      height: height,
      icon_size: icon_size,
      cell_width: cell_width,
      cell_height: cell_height
    } = icon_widget

    %{
      count: count,
      images: images,
      model: model,
      selected_index: selected_index,
      state: state
    } = ui_state[name]

    cols = div(width, cell_width)
    rows = div(height, cell_height)

    icons_per_page = cols * rows
    page = div(selected_index, icons_per_page)

    # TODO: maybe Enum.reduce_while
    {_, icons} =
      model
      |> Enum.slice(page * icons_per_page, icons_per_page)
      |> Enum.reduce({0, acc}, fn v, {index, grid_acc} ->
        img = images[index]
        txt = v[:text]
        x_pos = origin_x + x + cell_width * rem(index, cols)
        y_pos = origin_y + y + cell_height * div(index, cols)
        icon_xpos = x_pos + div(cell_width - icon_size, 2)
        icon_ypos = y_pos + 8
        text_xpos = x_pos + div(cell_width - byte_size(txt) * 8, 2)
        text_ypos = y_pos + 16 + icon_size

        {text_color, bg_color, with_background} =
          if index + page * icons_per_page == selected_index do
            color =
              if state == :released do
                0x0000FF
              else
                0x000080
              end

            {0xFFFFFF, color, [{:rect, x_pos, y_pos, cell_width, cell_height, color} | grid_acc]}
          else
            {0x000000, 0xFFFFFF, grid_acc}
          end

        {index + 1,
         [
           {:text, text_xpos, text_ypos, :default16px, text_color, bg_color, v[:text]},
           {:image, icon_xpos, icon_ypos, bg_color, img}
           | with_background
         ]}
      end)

    pages = div(count, icons_per_page) + 1

    if pages > 1 do
      scroll_height = div(height - 2, pages)
      scroll_yoff = scroll_height * page
      [{:rect, width - 6, origin_y + y + scroll_yoff, 2, scroll_height, 0x000000} | icons]
    else
      icons
    end
  end
end

defmodule PhotonUI.Widgets.IconGridViewState do
  defstruct [:images, :model, :count, :selected_index, :state]

  @compile {:no_warn_undefined, :atomvm}

  def init(_widget) do
    %__MODULE__{
      state: :released,
      selected_index: 0,
      images: %{},
      model: [],
      count: 0
    }
  end

  defp load_images(model) do
    model
    |> Enum.reduce({0, []}, fn item, {index, acc} ->
      {index + 1, [{index, ImageState.load_image(item[:source])} | acc]}
    end)
    |> elem(1)
    |> Enum.into(%{})
  end

  def update_property(%__MODULE__{} = s, property, value) do
    case property do
      :model ->
        %__MODULE__{s | model: value, count: Enum.count(value), images: load_images(value)}
    end
  end

  def handle_input(grid_state, {:keyboard, :up, :down}, _ts) do
    %{selected_index: selected_index, count: count} = grid_state

    next_index = rem(selected_index + 1, count)

    {%__MODULE__{grid_state | selected_index: next_index},
     [event: {:selected_item_changed, next_index}]}
  end

  def handle_input(grid_state, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    %__MODULE__{grid_state | state: :pressed}
  end

  def handle_input(grid_state, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    %__MODULE__{selected_index: selected_index, model: model} = grid_state

    item = Enum.at(model, selected_index)

    {%__MODULE__{grid_state | state: :released}, [event: {:clicked, selected_index, item}]}
  end

  def handle_input(button, event, ts) do
    PhotonUI.UIServer.default_input_handler(button, event, ts)
  end
end

defmodule PhotonUI.Widgets.Image do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :source]

  @bg_color 0xFFFFFF

  def state_struct_module(), do: ImageState
  def has_init_function(), do: true

  def render(icon_widget, name, ui_state, origin_x, origin_y, acc) do
    %Image{x: x, y: y} = icon_widget
    %{image: image} = ui_state[name]

    [{:image, origin_x + x, origin_y + y, @bg_color, image} | acc]
  end
end

defmodule PhotonUI.Widgets.ImageState do
  defstruct [:image]

  @compile {:no_warn_undefined, :atomvm}

  def init(%Image{source: source}) do
    %ImageState{
      image: load_image(source)
    }
  end

  def load_image({app, icon_name}) do
    <<"rgba8888", width::little-unsigned-size(16), height::little-unsigned-size(16),
      _tile_width::little-unsigned-size(16), _tile_height::little-unsigned-size(16),
      data::binary>> = :atomvm.read_priv(app, icon_name)

    {:rgba8888, width, height, data}
  end

  def load_image({:rgba8888, _, _, _} = image) do
    image
  end
end

defmodule PhotonUI.Widgets.Rectangle do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :color]

  @bg_color 0xFFFFFF

  def render(rect_widget, _name, _ui_state, origin_x, origin_y, acc) do
    %__MODULE__{x: x, y: y, width: width, height: height, color: color} = rect_widget
    [{:rect, origin_x + x, origin_y + y, width, height, color || @bg_color} | acc]
  end
end

defmodule PhotonUI.Widgets.Text do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :bgcolor, text: "", color: 0x000000]

  @bg_color 0xFFFFFF

  def render(text_widget, _name, _ui_state, origin_x, origin_y, acc) do
    %Text{text: text, x: x, y: y, color: color, bgcolor: bgcolor} = text_widget

    [
      {:text, origin_x + x, origin_y + y, :default16px, color, bgcolor || @bg_color, text} | acc
    ]
  end
end

defmodule PhotonUI.Widgets.TextInput do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height]

  @bg_color 0xFFFFFF

  def can_be_focused?(_widget, _state), do: true
  def state_struct_module(), do: TextInputState

  def render(text_input, name, ui_state, origin_x, origin_y, acc) do
    %TextInput{x: x, y: y} = text_input
    %{cursor_pos: cursor_pos, text: text} = ui_state[name]

    x_pos = origin_x + x
    y_pos = origin_y + y

    list = [
      {:text, x_pos, y_pos, :default16px, 0x000000, @bg_color, text}
      | acc
    ]

    if ui_state[:"$focused_item"] == name do
      [{:rect, x_pos + cursor_pos * 8, y_pos, 2, 16, 0x000000} | list]
    else
      list
    end
  end
end

defmodule PhotonUI.Widgets.TextInputState do
  defstruct text: "",
            cursor_pos: 0

  def update_property(%TextInputState{} = s, property, value) do
    case property do
      :text ->
        %TextInputState{s | text: to_string(value), cursor_pos: 0}
    end
  end

  def handle_input(_text_input_state, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    :none
  end

  def handle_input(_text_input_state, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    :release_focus
  end

  def handle_input(text_input_state, {:keyboard, :down, code}, _ts) when is_integer(code) do
    %TextInputState{text: old_text, cursor_pos: old_cursor_pos} = text_input_state

    {updated_text, updated_cursor_pos} = update_text(old_text, code, old_cursor_pos)

    {%TextInputState{text_input_state | text: updated_text, cursor_pos: updated_cursor_pos},
     [event: {:text_input, updated_text}]}
  end

  def handle_input(text_input_state, event, ts) do
    PhotonUI.UIServer.default_input_handler(text_input_state, event, ts)
  end

  def update_text(text, ?\b, 0) do
    {text, 0}
  end

  def update_text(text, char, cursor_pos) do
    case char do
      8 ->
        case text do
          <<pre::binary-size(cursor_pos - 1), _remove::size(8), rest::binary>> ->
            {<<pre::binary, rest::binary>>, cursor_pos - 1}

          <<>> ->
            {<<>>, 0}
        end

      _ ->
        <<pre::binary-size(cursor_pos), rest::binary>> = text
        {<<pre::binary, char::size(8), rest::binary>>, cursor_pos + 1}
    end
  end
end

defmodule PhotonUI.Widgets.VerticalLayout do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height, :children, spacing: 0]

  def render(vertical_layout, _name, ui_state, origin_x, origin_y, acc) do
    %VerticalLayout{children: children, x: x, y: y, spacing: spacing} = vertical_layout

    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_y}, fn %{y: wy, height: wh} = item, {rendered, y_off} ->
        {UIServer.render_widgets([item], ui_state, origin_x + x, y_off + y, rendered),
         y_off + wy + wh + spacing}
      end)

    acc_with_children
  end

  def prepend_mouse_area(widget, ui_state, origin_x, origin_y, acc) do
    %VerticalLayout{children: children, x: x, y: y, spacing: spacing} = widget

    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_y}, fn %{y: wy, height: wh} = item,
                                                {mouse_area_acc, y_off} ->
        {UIServer.build_mouse_area_list(
           [item],
           ui_state,
           origin_x + x,
           y_off + y,
           mouse_area_acc
         ), y_off + wy + wh + spacing}
      end)

    acc_with_children
  end
end

defmodule PhotonUI.UIServer do
  require Record
  Record.defrecord(:ui_server_state, [:module, :ui, :custom_state])

  @compile {:no_warn_undefined, :port}

  @bg_color 0xFFFFFF

  def start(module, args, opts) do
    :avm_scene.start(__MODULE__, [{module, opts} | args], opts)
  end

  def start_link(module, args, opts) do
    :avm_scene.start_link(__MODULE__, [{module, opts} | args], opts)
  end

  def start_monitor(module, args, opts) do
    :avm_scene.start_monitor(__MODULE__, [{module, opts} | args], opts)
  end

  def init([{module, opts} | args]) do
    {:port, disp} = opts[:display_server]
    :port.call(disp, {:subscribe_input, :all})

    case opts[:keyboard_server] do
      nil ->
        :ok

      servers when is_list(servers) ->
        Enum.each(servers, fn server ->
          IO.puts("Subscribed to keyboard server.")
          :gen_server.call(server, {:subscribe_input, :all})
        end)
    end

    {:ok, {widgets, _initial_widget_state}, custom_state} = module.init(args)

    # TODO: merge with provided widget initial state
    ui_state =
      build_ui_state(widgets, %{
        width: opts[:width],
        height: opts[:height],
        visible: false
      })

    s =
      ui_server_state(
        module: module,
        ui: {widgets, ui_state},
        custom_state: custom_state
      )

    {:ok, s}
  end

  defp build_ui_state(widgets, assigned_state) do
    built_initial_state = make_initial_state(widgets, assigned_state)

    focus_list = build_focus_list(widgets, built_initial_state)
    focused_item = Enum.at(focus_list, 0)
    mouse_area_list = build_mouse_area_list(widgets, built_initial_state)

    ui_state =
      built_initial_state
      |> Map.put(:"$focused_item", focused_item)
      |> Map.put(:"$focus_list", focus_list)
      |> Map.put(:"$mouse_area_list", mouse_area_list)

    ui_state
  end

  def handle_call({:ui_server, :show}, _from, state) do
    module = ui_server_state(state, :module)
    {widgets, widget_state} = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    shown_ui = {widgets, Map.put(widget_state, :visible, true)}

    case module.handle_event(:ui, :shown, shown_ui, custom_state) do
      {:noreply, new_custom_state} ->
        {:reply, :ok, shown_ui, new_custom_state}

      {:noreply, new_ui, new_custom_state} ->
        {:reply, :ok, new_ui, new_custom_state}
    end
    |> to_avm_scene_result(state)
  end

  def handle_call(msg, from, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_call(msg, from, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_cast(msg, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_cast(msg, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_info(msg, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_info(msg, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_input(event_data, ts, _pid, state) do
    {_widgets, widget_state} = ui_server_state(state, :ui)

    case event_data do
      {:mouse, _mouse_evt, _button, x, y} ->
        # TODO: propagate widget coordinates instead of screen coordinates
        # this can be easily done by subtracting hit mouse area x and y
        find_mouse_area(widget_state[:"$mouse_area_list"], x, y)
        |> dispatch_input(event_data, ts, state)

      {:keyboard, _up_down, _code} ->
        widget_state[:"$focused_item"]
        |> dispatch_input(event_data, ts, state)

      _ ->
        {:noreply, state}
    end
  end

  defp dispatch_input(widget_name, event_data, ts, state) do
    module = ui_server_state(state, :module)
    {widgets, widget_state} = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    case widget_state[widget_name] do
      nil ->
        {:noreply, state}

      %wdg_state_type{} = wdg_state ->
        case wdg_state_type.handle_input(wdg_state, event_data, ts) do
          %^wdg_state_type{} = new_wdg_state ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)
            display_list = render(widgets, new_widget_state)

            {:noreply, ui_server_state(state, ui: {widgets, new_widget_state}),
             [push: display_list]}

          {%^wdg_state_type{} = new_wdg_state, [event: wdg_event]} ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)

            case module.handle_event(
                   widget_name,
                   wdg_event,
                   {widgets, new_widget_state},
                   custom_state
                 ) do
              {:noreply, new_custom_state} ->
                {:noreply, {widgets, new_widget_state}, new_custom_state}

              {:noreply, _new_ui, _new_custom_state} = t ->
                t

              {:stop, reason, new_custom_state} ->
                {:stop, reason, {widgets, new_widget_state}, new_custom_state}
            end
            |> to_avm_scene_result(state)

          {%^wdg_state_type{} = new_wdg_state, :release_focus} ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)
            focus_next(ui_server_state(ui: {widgets, new_widget_state}))

          :release_focus ->
            focus_next(state)

          :none ->
            {:noreply, state}
        end

      _other ->
        {:noreply, state}
    end
  end

  def default_input_handler(_widget, event, _ts) do
    case event do
      # SDL down arrow
      {:keyboard, :up, :down} -> :release_focus
      _ -> :none
    end
  end

  defp focus_next(state) do
    {items, widget_state} = ui_server_state(state, :ui)

    new_focused_item =
      find_next_focusable(widget_state[:"$focus_list"], widget_state[:"$focused_item"])

    new_state = Map.put(widget_state, :"$focused_item", new_focused_item)

    display_list = render(items, new_state)

    {:noreply, ui_server_state(state, ui: {items, new_state}), [push: display_list]}
  end

  defp make_initial_state([], acc) do
    acc
  end

  defp make_initial_state([%widget_type{name: name} = widget | t], acc) do
    acc_with_children =
      case widget do
        %{children: children} -> make_initial_state(children, acc)
        _ -> acc
      end

    acc_with_widget_state =
      cond do
        match?(%{^name => _state}, acc_with_children) ->
          # This is required to avoid replacing existing state when initializing again an existing
          # state
          acc_with_children

        function_exported?(widget_type, :has_init_function, 0) ->
          struct_name = widget_type.state_struct_module()
          initialized = struct_name.init(widget)
          Map.put(acc_with_children, name, initialized)

        function_exported?(widget_type, :state_struct_module, 0) ->
          struct_name = widget_type.state_struct_module()
          Map.put(acc_with_children, name, struct(struct_name))

        true ->
          acc_with_children
      end

    make_initial_state(t, acc_with_widget_state)
  end

  def render_widgets([], _state, _origin_x, _origin_y, acc) do
    acc
  end

  def render_widgets([%widget_type{name: name} = widget | t], state, origin_x, origin_y, acc) do
    new_rendered_acc = widget_type.render(widget, name, state, origin_x, origin_y, acc)
    render_widgets(t, state, origin_x, origin_y, new_rendered_acc)
  end

  def build_mouse_area_list(widgets, ui_state) do
    build_mouse_area_list(widgets, ui_state, 0, 0, [])
  end

  def build_mouse_area_list([], _ui_state, _origin_x, _origin_y, acc) do
    Enum.reverse(acc)
  end

  def build_mouse_area_list(
        [%widget_type{name: name} = widget | t],
        ui_state,
        origin_x,
        origin_y,
        acc
      ) do
    cond do
      function_exported?(widget_type, :accepts_mouse_events, 2) ->
        if widget_type.accepts_mouse_events(widget, ui_state) do
          %{x: x, y: y, width: width, height: height} = widget

          build_mouse_area_list(t, ui_state, origin_x, origin_y, [
            {name, origin_x + x, origin_y + y, width, height} | acc
          ])
        else
          build_mouse_area_list(t, ui_state, origin_x, origin_y, acc)
        end

      function_exported?(widget_type, :prepend_mouse_area, 5) ->
        acc_with_children =
          widget_type.prepend_mouse_area(widget, ui_state, origin_x, origin_y, acc)

        build_mouse_area_list(t, ui_state, origin_x, origin_y, acc_with_children)

      match?(%{children: _children, x: _x, y: _y}, widget) ->
        %{children: children, x: x, y: y} = widget

        children_focus_list =
          build_mouse_area_list(children, ui_state, origin_x + x, origin_y + y, acc)

        build_mouse_area_list(t, ui_state, origin_x, origin_y, children_focus_list)

      true ->
        build_mouse_area_list(t, ui_state, origin_x, origin_y, acc)
    end
  end

  def find_mouse_area(mouse_area_list, x, y) do
    ### TODO: find/2 !!!
    {widget_name, _w_x, _w_y, _widget_width, _widget_height} =
      Enum.find(mouse_area_list, {nil, 0, 0, 0, 0}, fn {_wn, wx, wy, ww, wh} ->
        x >= wx and x < wx + ww and y >= wy and y < wy + wh
      end)

    widget_name
  end

  def build_focus_list(widgets, ui_state) do
    build_focus_list(widgets, ui_state, [])
  end

  def build_focus_list([], _ui_state, acc) do
    Enum.reverse(acc)
  end

  def build_focus_list([widget | t], ui_state, acc) do
    %widget_type{name: name} = widget

    cond do
      function_exported?(widget_type, :can_be_focused?, 2) and
          widget_type.can_be_focused?(widget, ui_state) ->
        build_focus_list(t, ui_state, [name | acc])

      match?(%{children: _children}, widget) ->
        children_focus_list =
          widget.children
          |> build_focus_list(ui_state, [])
          |> Enum.reverse()

        build_focus_list(t, ui_state, children_focus_list ++ acc)

      true ->
        build_focus_list(t, ui_state, acc)
    end
  end

  def find_next_focusable(focus_list, focused) do
    index = Enum.find_index(focus_list, fn item -> item == focused end)

    case Enum.at(focus_list, index + 1) do
      nil -> Enum.at(focus_list, 0)
      next -> next
    end
  end

  defp to_avm_scene_result(result, state) do
    case result do
      {:reply, reply, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:reply, reply, updated_state, [{:push, rendered_items}]}

      {:reply, reply, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:reply, reply, updated_state}

      {:noreply, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:noreply, updated_state, [{:push, rendered_items}]}

      {:noreply, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:noreply, updated_state}

      {:stop, reason, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:stop, reason, updated_state, [{:push, rendered_items}]}

      {:stop, reason, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:stop, reason, updated_state}
    end
  end

  defp update_ui_and_state(old_state, new_widgets, new_widget_state, custom_state) do
    rendered_items = render(new_widgets, new_widget_state)

    updated_state =
      ui_server_state(old_state, ui: {new_widgets, new_widget_state}, custom_state: custom_state)

    {updated_state, rendered_items}
  end

  def begin_widget_state_update({_widgets, widget_state}) do
    widget_state
  end

  def update_property!(widget_state, name, property, value)
      when is_map(widget_state) and is_atom(property) do
    %{^name => %widget_type{} = individual_widget_state} = widget_state

    updated_individual_widget_state =
      widget_type.update_property(individual_widget_state, property, value)

    %{widget_state | name => updated_individual_widget_state}
  end

  def apply_widget_state_update(updated_widget_state, {widgets, _widget_state}) do
    {widgets, updated_widget_state}
  end

  def get_widget_state({_widgets, widget_state}) do
    widget_state
  end

  def get_property!(ui, name, property) when is_atom(property) do
    case ui do
      {_widgets, %{^name => %{^property => value}}} ->
        value

      {widgets, _widget_state} ->
        %_struct{^property => value} = find_widget(widgets, name)
        value
    end
  end

  def find_widget({widgets, _widgets_name}, name) do
    find_widget(widgets, name)
  end

  def find_widget([], _name) do
    nil
  end

  def find_widget([%{name: name} = widget | _t], name) do
    widget
  end

  def find_widget([_widget_name, %{children: children} | t], name) do
    case find_widget(children, name) do
      nil -> find_widget(t, name)
      found_in_children -> found_in_children
    end
  end

  def find_widget([{_widget_name, _widget} | t], name) do
    find_widget(t, name)
  end

  def render(widgets, widget_state) do
    if widget_state[:visible] do
      render_widgets(widgets, widget_state, 0, 0, [
        {:rect, 0, 0, widget_state[:width], widget_state[:height], @bg_color}
      ])
    else
      []
    end
  end

  def replace_ui({_old_widgets, old_ui_state}, new_widgets) do
    new_ui_state = build_ui_state(new_widgets, old_ui_state)
    {new_widgets, new_ui_state}
  end

  # Public API
  def show(ui_server) do
    :gen_server.call(ui_server, {:ui_server, :show})
  end
end
