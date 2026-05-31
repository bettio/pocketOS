alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.UIServer

defmodule UI.MeshNodes do
  def get_ui do
    avail_mem =
      try do
        "#{div(:erlang.system_info(:esp32_free_heap_size), 1024)} K"
      rescue
        _ -> "~~~ K"
      end

    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 1,
        children: [
          %Container{
            name: :title_bar,
            x: 0,
            y: 0,
            width: 320,
            height: 16,
            children: [
              %Rectangle{
                name: :title_label_bg,
                x: 0,
                y: 0,
                height: 16,
                width: 320,
                color: 0x000000
              },
              %Text{
                name: :title_label,
                x: 8,
                y: 0,
                height: 16,
                width: byte_size(" Mesh Messages ") * 8,
                text: " Mesh Messages ",
                bgcolor: 0x4792EC
              },
              %Text{
                name: :memory_label,
                x: 320 - byte_size(avail_mem) * 8 - 4,
                y: 0,
                height: 16,
                width: byte_size(avail_mem) * 8,
                text: avail_mem,
                color: 0xFFFFFF,
                bgcolor: 0x000000
              }
            ]
          },
          %IconListView{
            name: :grid,
            x: 0,
            y: 0,
            height: 235,
            width: 320,
            icon_size: 32,
            cell_height: 40
          }
        ]
      }
    ]
  end

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def start_monitor(args, opts) do
    UIServer.start_monitor(__MODULE__, args, opts)
  end

  def init(_opts) do
    {:ok, {get_ui(), %{}}, %{page: :main}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info({:mnesia_table_event, {:write, _, _}}, ui, state) do
    {updated_ui, new_state} = reload_model(ui, state)
    {:noreply, updated_ui, new_state}
  end

  def handle_info(msg, _ui, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, ui, state) do
    :micronesia.subscribe({:table, :meshtastic_node_info, :simple})
    :micronesia.subscribe({:table, :meshtastic_position, :simple})
    :micronesia.subscribe({:table, :meshtastic_signal, :simple})

    {updated_ui, new_state} = reload_model(ui, state)

    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :exit} = _item}, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: node_id} = _item}, ui, state) do
    {updated_ui, new_state} = reload_model(ui, state)
    {:noreply, updated_ui, Map.put(new_state, :page, [:node_info, node_id])}
  end

  def handle_event(:send_button, :clicked, ui, state) do
    text = UIServer.get_property!(ui, :message_input, :text)

    MeshtasticCallbacks.send_text_message(text)

    {:noreply, UIServer.replace_ui(ui, get_ui()), state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: _id} = _item}, _ui, state) do
    {:noreply, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  defp reload_list(ui, %{page: [:node_info, node_id]} = state) do
    node_info_list =
      case :micronesia.dirty_read({:meshtastic_node_info, node_id}) do
        [{:meshtastic_node_info, node_id, user_info_map}] ->
          Enum.map(user_info_map, fn {key, value} ->
            %{
              id: key,
              text: "#{key}: #{maybe_encode(key, value)}",
              source: {:pocket_os, "icons/32/generic/info.rgba"}
            }
          end)

        [] ->
          []
      end

    position_list =
      case :micronesia.dirty_read({:meshtastic_position, node_id}) do
        [{:meshtastic_position, node_id, position_map}] ->
          Enum.map(position_map, fn {key, value} ->
            %{
              id: key,
              text: "#{key}: #{value}",
              source: {:pocket_os, "icons/32/generic/info.rgba"}
            }
          end)

        [] ->
          []
      end

    signal_list =
      case :micronesia.dirty_read({:meshtastic_signal, node_id}) do
        [{:meshtastic_signal, node_id, signal_map}] ->
          Enum.map(signal_map, fn {key, value} ->
            %{
              id: key,
              text: "#{key}: #{maybe_encode(key, value)}",
              source: {:pocket_os, "icons/32/generic/info.rgba"}
            }
          end)

        [] ->
          []
      end

    node_info_list ++ position_list ++ signal_list
  end

  defp reload_list(ui, state) do
    :micronesia.all(:meshtastic_node_info)
    |> Enum.map(fn {:meshtastic_node_info, node_id, payload} ->
      case payload do
        %{id: id, short_name: short_name} ->
          %{
            id: node_id,
            text: "#{short_name} | #{id}",
            source: {:pocket_os, "icons/32/generic/new_mail.rgba"}
          }

        _ ->
          %{
            id: node_id,
            text: "node_id: #{node_id}",
            source: {:pocket_os, "icons/32/generic/new_mail.rgba"}
          }
      end
    end)
  end

  defp reload_model(ui, state) do
    the_list = reload_list(ui, state)

    exit = %{
      id: :exit,
      text: "Exit",
      source: {:pocket_os, "icons/32/generic/go_back.rgba"}
    }

    list_model = [exit | the_list]

    updated_ui =
      UIServer.begin_widget_state_update(ui)
      |> UIServer.update_property!(:grid, :model, list_model)
      |> UIServer.apply_widget_state_update(ui)

    {updated_ui, state}
  end

  defp maybe_encode(key, value) when key in [:macaddr, :public_key] do
    Base.encode16(value)
  end

  defp maybe_encode(:last_heard, value) do
    format_ago(value)
  end

  defp maybe_encode(_key, value) do
    value
  end

  defp format_ago(epoch) when is_integer(epoch) do
    delta = :erlang.system_time(:second) - epoch

    cond do
      delta < 0 -> "0s ago"
      delta < 60 -> "#{delta}s ago"
      delta < 3600 -> "#{div(delta, 60)}m ago"
      delta < 86_400 -> "#{div(delta, 3600)}h ago"
      true -> "#{div(delta, 86_400)}d ago"
    end
  end

  defp format_ago(_), do: "?"
end
