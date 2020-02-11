defmodule InputBox do
  
  def init() do
    [
      input_state: init_state()
    ]
  end

  def render(state) do
    value =
      state
      |> Keyword.get(:input_state, [])
      |> Keyword.get(:buffer)

    render(:icons64.critical_icon(), "Title", "Text", value)
  end

  def render(icon, title, text, value) do
    [
      {:clear_screen, 0xCE59},
      {:rect, 0, 0, 320, 18, 0x0010},
      {:text, 1, 1, title, 0xFFFF},
      {:image, 1, 80, icon, 0xCE59},
      {:text, 70, 103, text, 0x00},
      {:text, 70, 150, value, 0x00}
    ]
  end

  def handle({:keyboard_event, _keycode, _keypress, _ts} = event, state) do
    input_state = Keyword.get(state, :input_state, [])
    new_input_state = KeyboardParser.process_event(input_state, event)
    new_state = Keyword.put(state, :input_state, new_input_state)

    new_scene = render(new_state)

    {:noreply_render, new_state, new_scene}
  end

  def init_state do
    [
      buffer: 'test',
      last_timestamp: 0,
      upper_case: false,
      last_key: nil,
      char_index: 0,
      key_down: false
    ]
  end
end
