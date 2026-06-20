alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.UIServer

defmodule UI.Traceroute do
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
                width: byte_size(" Traceroute ") * 8,
                text: " Traceroute ",
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
    {:ok, {get_ui(), %{}}, %{page: :main, sent_at: %{}}}
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
    :micronesia.subscribe({:table, :meshtastic_traceroute, :simple})
    :micronesia.subscribe({:table, :meshtastic_node_info, :simple})

    {updated_ui, new_state} = reload_model(ui, state)

    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :exit}}, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :back}}, ui, state) do
    {updated_ui, new_state} = reload_model(ui, %{state | page: :main})
    {:noreply, updated_ui, new_state}
  end

  def handle_event(
        :grid,
        {:clicked, _index, %{id: :trace_again}},
        ui,
        %{page: [:report, node_id]} = state
      ) do
    new_state = start_trace(node_id, state)
    {updated_ui, new_state} = reload_model(ui, new_state)
    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: node_id}}, ui, state)
      when is_integer(node_id) do
    new_state = start_trace(node_id, state)
    {updated_ui, new_state} = reload_model(ui, new_state)
    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, _item}, _ui, state) do
    {:noreply, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  defp start_trace(node_id, state) do
    MeshtasticCallbacks.send_traceroute(node_id)
    sent_at = Map.put(state.sent_at, node_id, :erlang.system_time(:second))
    %{state | page: [:report, node_id], sent_at: sent_at}
  end

  defp reload_list(_ui, %{page: [:report, node_id]} = state) do
    sent_at = Map.get(state.sent_at, node_id, 0)

    control = [
      %{id: :back, text: "Back", source: {:pocket_os, "icons/32/generic/go_back.rgba"}},
      %{
        id: :trace_again,
        text: "Trace again",
        source: {:pocket_os, "icons/32/generic/beacon.rgba"}
      }
    ]

    body =
      case :micronesia.dirty_read({:meshtastic_traceroute, node_id}) do
        [{:meshtastic_traceroute, ^node_id, %{received_at: rx_at} = report}]
        when rx_at >= sent_at ->
          report_rows(node_id, report)

        _ ->
          [info_row("To: #{node_label(node_id)}"), info_row("Tracing...")]
      end

    control ++ body
  end

  defp reload_list(_ui, _state) do
    :micronesia.all(:meshtastic_node_info)
    |> Enum.map(fn {:meshtastic_node_info, node_id, payload} ->
      label =
        case payload do
          %{id: id, short_name: short_name} -> "#{short_name} | #{id}"
          _ -> "node_id: #{node_id}"
        end

      %{id: node_id, text: label, source: {:pocket_os, "icons/32/generic/new_mail.rgba"}}
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

  defp report_rows(dest_node_id, report) do
    route = Map.get(report, :route, [])
    snr_towards = Map.get(report, :snr_towards, [])
    route_back = Map.get(report, :route_back, [])
    snr_back = Map.get(report, :snr_back, [])
    reply_snr = Map.get(report, :reply_snr)
    reply_rssi = Map.get(report, :reply_rssi)
    received_at = Map.get(report, :received_at)

    forward = path_rows("->", route ++ [dest_node_id], snr_towards)

    return =
      case route_back do
        [] ->
          [info_row("<- direct, rx #{raw_snr_label(reply_snr)}")]

        _ ->
          path_rows("<-", route_back, snr_back) ++
            [info_row("<- us, rx #{raw_snr_label(reply_snr)}")]
      end

    [info_row("To: #{node_label(dest_node_id)}")] ++
      [info_row("received #{format_ago(received_at)}")] ++
      [info_row("Forward:")] ++
      forward ++
      [info_row("Return:")] ++
      return ++
      [info_row("link rssi #{raw_rssi_label(reply_rssi)}")]
  end

  defp path_rows(_arrow, [], _snrs), do: []

  defp path_rows(arrow, [nid | nids], snrs) do
    {snr, rest} =
      case snrs do
        [s | r] -> {s, r}
        [] -> {nil, []}
      end

    [info_row("#{arrow} #{hop_label(nid, snr)}") | path_rows(arrow, nids, rest)]
  end

  defp info_row(text) do
    %{id: :info, text: text, source: {:pocket_os, "icons/32/generic/info.rgba"}}
  end

  defp hop_label(0xFFFFFFFF, snr), do: "(unknown) #{wire_snr_label(snr)}"
  defp hop_label(nid, snr), do: "#{node_label(nid)} #{wire_snr_label(snr)}"

  defp node_label(nid) do
    hex = "!" <> Base.encode16(<<nid::32>>)

    case :micronesia.dirty_read({:meshtastic_node_info, nid}) do
      [{:meshtastic_node_info, ^nid, %{short_name: short_name}}] -> "#{short_name} #{hex}"
      _ -> hex
    end
  end

  defp wire_snr_label(nil), do: "?"
  defp wire_snr_label(-128), do: "?"
  defp wire_snr_label(b) when is_integer(b), do: "#{round(b / 4)}dB"
  defp wire_snr_label(_), do: "?"

  defp raw_snr_label(v) when is_integer(v), do: "#{v}dB"
  defp raw_snr_label(_), do: "?"

  defp raw_rssi_label(v) when is_integer(v), do: "#{v}dBm"
  defp raw_rssi_label(_), do: "?"

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
