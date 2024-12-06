alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.TextInputState
alias PhotonUI.UIServer
alias UI.Terminal.TermWidget

defmodule UI.Terminal do
  defstruct [:fpid, :fref, :pre_input_buffer, :pre_input_col_pos, line_buffer: ""]

  @compile {:no_warn_undefined, :arepl}

  @ui [
    %Container{
      name: :grid,
      x: 0,
      y: 0,
      width: 320,
      height: 240,
      children: [
        %TermWidget{
          name: :term_widget,
          x: 1,
          y: 1,
          height: 240,
          width: 320
        }
      ]
    }
  ]

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def start_monitor(args, opts) do
    UIServer.start_monitor(__MODULE__, args, opts)
  end

  def init([{:mf, {module, function}} | _t] = _opts) do
    leader = self()

    spawn(fn ->
      :erlang.group_leader(leader, self())
      apply(module, function, [])
    end)

    {:ok, {@ui, %{}}, %__MODULE__{}}
  end

  def init([{:mfa, {module, function, args}} | _t] = _opts) do
    leader = self()

    spawn(fn ->
      :erlang.group_leader(leader, self())
      apply(module, function, args)
    end)

    {:ok, {@ui, %{}}, %__MODULE__{}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info({:io_request, fpid, fref, request}, ui, state) do
    io_request(request, fpid, fref, ui, state)
  end

  def handle_info(_msg, _ui, state) do
    {:noreply, state}
  end

  def handle_event(:term_widget, {:got_char, ?\n}, ui, state) do
    %__MODULE__{line_buffer: line_buffer, fpid: fpid, fref: fref} = state

    {new_line_buffer, _edit_pos} =
      TextInputState.update_text(line_buffer, ?\n, byte_size(line_buffer))

    updated_ui =
      do_unicode_write([?\n], ui)
      |> UIServer.begin_widget_state_update()
      |> UIServer.update_property!(:term_widget, :accepting_input, false)
      |> UIServer.apply_widget_state_update(ui)

    reply = {:io_reply, fref, :unicode.characters_to_list(new_line_buffer)}
    send(fpid, reply)

    {:noreply, updated_ui, %__MODULE__{state | line_buffer: "", fpid: nil, fref: nil}}
  end

  def handle_event(:term_widget, {:got_char, char}, ui, state) do
    %__MODULE__{
      line_buffer: line_buffer,
      pre_input_buffer: pre_input_buffer,
      pre_input_col_pos: pre_input_col_pos
    } = state

    {new_line_buffer, _edit_pos} =
      TextInputState.update_text(line_buffer, char, byte_size(line_buffer))

    pre_input_buffer_ui =
      ui
      |> UIServer.begin_widget_state_update()
      |> UIServer.update_property!(:term_widget, :buffer, pre_input_buffer)
      |> UIServer.update_property!(:term_widget, :cursor_col_pos, pre_input_col_pos)
      |> UIServer.apply_widget_state_update(ui)

    updated_ui = do_unicode_write(new_line_buffer, pre_input_buffer_ui)

    {:noreply, updated_ui, %__MODULE__{state | line_buffer: new_line_buffer}}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  def io_request({:get_line, :unicode, data}, fpid, fref, ui, state) do
    updated_ui =
      do_unicode_write(data, ui)
      |> UIServer.begin_widget_state_update()
      |> UIServer.update_property!(:term_widget, :accepting_input, true)
      |> UIServer.apply_widget_state_update(ui)

    pre_input_buffer = UIServer.get_property!(updated_ui, :term_widget, :buffer)
    pre_input_col_pos = UIServer.get_property!(updated_ui, :term_widget, :cursor_col_pos)

    {:noreply, updated_ui,
     %__MODULE__{
       state
       | fpid: fpid,
         fref: fref,
         pre_input_buffer: pre_input_buffer,
         pre_input_col_pos: pre_input_col_pos
     }}
  end

  def io_request({:put_chars, :unicode, data}, fpid, fref, ui, state) do
    updated_ui = do_unicode_write(data, ui)
    send(fpid, {:io_reply, fref, :ok})
    {:noreply, updated_ui, state}
  end

  defp split_lines(lines_list, initial_col) do
    columns = 40

    chunk_fun = fn char, {count, rchars} ->
      cond do
        char == ?\n -> {:cont, Enum.reverse(rchars), {0, []}}
        count == columns -> {:cont, Enum.reverse(rchars), {1, [char]}}
        true -> {:cont, {count + 1, [char | rchars]}}
      end
    end

    after_fun = fn
      {_count, []} -> {:cont, [], []}
      {_count, rchars} -> {:cont, Enum.reverse(rchars), []}
    end

    Enum.chunk_while(lines_list, {initial_col, []}, chunk_fun, after_fun)
  end

  defp do_unicode_write(unicode_data, ui) do
    old_buffer = UIServer.get_property!(ui, :term_widget, :buffer)
    old_cursor_col_pos = UIServer.get_property!(ui, :term_widget, :cursor_col_pos)

    reverse_chunks =
      unicode_data
      |> :unicode.characters_to_list()
      |> split_lines(old_cursor_col_pos)
      |> Enum.reduce([], fn line, acc -> [:unicode.characters_to_binary(line) | acc] end)

    [last_line | other_lines] = old_buffer
    first_new_line = List.last(reverse_chunks)
    other_new_lines = Enum.slice(reverse_chunks, 0, length(reverse_chunks) - 1)

    new_buffer =
      (other_new_lines ++ [last_line <> first_new_line | other_lines])
      |> Enum.slice(0, 15)

    [new_last_line | _] = new_buffer
    # TODO: String.length
    cursor_col_pos =
      new_last_line
      |> :unicode.characters_to_list()
      |> length()

    UIServer.begin_widget_state_update(ui)
    |> UIServer.update_property!(:term_widget, :buffer, new_buffer)
    |> UIServer.update_property!(:term_widget, :cursor_col_pos, cursor_col_pos)
    |> UIServer.apply_widget_state_update(ui)
  end
end
