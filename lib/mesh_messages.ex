alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.UIServer

defmodule UI.MeshMessages do
  @result_ms 10_000
  @send_failsafe_ms 60_000

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

  defp compose_ui(to_label) do
    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 1,
        children: [
          %Button{
            name: :pick_recipient,
            x: 0,
            y: 0,
            height: 16,
            width: 320,
            text: "To: " <> to_label
          },
          %Text{
            name: :message_label,
            text: "Message: ",
            height: 16,
            x: 0,
            y: 0
          },
          %TextInput{
            name: :message_input,
            x: 0,
            y: 0,
            height: 16,
            width: 320
          },
          %Button{
            name: :send_button,
            x: 0,
            y: 0,
            height: 32,
            width: 48,
            text: "Send"
          }
        ]
      }
    ]
  end

  defp picker_ui do
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
                width: byte_size(" Pick recipient ") * 8,
                text: " Pick recipient ",
                bgcolor: 0x4792EC
              }
            ]
          },
          %IconListView{
            name: :recipients,
            x: 0,
            y: 0,
            height: 220,
            width: 320,
            icon_size: 32,
            cell_height: 40
          }
        ]
      }
    ]
  end

  defp status_ui(title, lines) do
    texts = status_texts(lines, [:status_line_0, :status_line_1, :status_line_2])

    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 8,
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
                width: byte_size(title) * 8,
                text: title,
                bgcolor: 0x4792EC
              }
            ]
          }
          | texts
        ]
      }
    ]
  end

  # TODO: fold into an Enum.with_index/1 pipeline once AtomVM's Enum has it
  defp status_texts([], _names), do: []

  defp status_texts([line | rest], [name | names]) do
    [
      %Text{
        name: name,
        x: 8,
        y: 0,
        height: 16,
        width: max(byte_size(line) * 8, 8),
        text: line
      }
      | status_texts(rest, names)
    ]
  end

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def start_monitor(args, opts) do
    UIServer.start_monitor(__MODULE__, args, opts)
  end

  def init(_opts) do
    {:ok, {get_ui(), %{}},
     %{recipient: :broadcast, draft: "", screen: :inbox, pending_ref: nil, result: nil}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info({:mnesia_table_event, {:write, _, _}}, ui, %{screen: :inbox} = state) do
    {updated_ui, new_state} = reload_model(ui, state)
    {:noreply, updated_ui, new_state}
  end

  def handle_info({:mnesia_table_event, _}, _ui, state) do
    {:noreply, state}
  end

  def handle_info(
        {:delivery_update, ref, status},
        ui,
        %{screen: :sending, pending_ref: ref} = state
      ) do
    case status do
      {:ack, info} -> show_result(ui, state, :success, ack_text(state.recipient, info))
      {:nak, reason} -> show_result(ui, state, :failure, "Failed: #{reason}")
    end
  end

  def handle_info({:delivery_update, _ref, _status}, _ui, state) do
    {:noreply, state}
  end

  def handle_info({:send_failsafe, ref}, ui, %{screen: :sending, pending_ref: ref} = state) do
    show_result(ui, state, :failure, "Failed: no response")
  end

  def handle_info({:send_failsafe, _ref}, _ui, state) do
    {:noreply, state}
  end

  def handle_info(:result_done, ui, %{screen: :result, result: :success} = state) do
    state = %{state | recipient: :broadcast, draft: "", screen: :inbox, result: nil}
    {updated_ui, new_state} = reload_model(UIServer.replace_ui(ui, get_ui()), state)
    {:noreply, updated_ui, new_state}
  end

  def handle_info(:result_done, ui, %{screen: :result, result: :failure} = state) do
    state = %{state | screen: :compose, result: nil}
    {:noreply, show_compose(ui, state), state}
  end

  def handle_info(:result_done, _ui, state) do
    {:noreply, state}
  end

  def handle_info(msg, _ui, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, ui, state) do
    :micronesia.subscribe({:table, :meshtastic_message, :simple})

    {updated_ui, new_state} = reload_model(ui, %{state | screen: :inbox})

    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :exit} = _item}, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :compose} = _item}, ui, state) do
    state = %{state | recipient: :broadcast, draft: "", screen: :compose}
    {:noreply, show_compose(ui, state), state}
  end

  def handle_event(:pick_recipient, :clicked, ui, state) do
    state = %{
      state
      | draft: UIServer.get_property!(ui, :message_input, :text),
        screen: :picker
    }

    ui =
      ui
      |> UIServer.replace_ui(picker_ui())
      |> set_recipient_model()

    {:noreply, ui, state}
  end

  def handle_event(:recipients, {:clicked, _index, %{id: :cancel}}, ui, state) do
    state = %{state | screen: :compose}
    {:noreply, show_compose(ui, state), state}
  end

  def handle_event(:recipients, {:clicked, _index, %{id: :broadcast}}, ui, state) do
    state = %{state | recipient: :broadcast, screen: :compose}
    {:noreply, show_compose(ui, state), state}
  end

  def handle_event(:recipients, {:clicked, _index, %{id: node_id, label: label}}, ui, state) do
    state = %{state | recipient: {node_id, label}, screen: :compose}
    {:noreply, show_compose(ui, state), state}
  end

  def handle_event(:send_button, :clicked, ui, state) do
    text = UIServer.get_property!(ui, :message_input, :text)
    state = %{state | draft: text}

    case send_async_to(state.recipient, text) do
      {:ok, ref} ->
        :erlang.send_after(@send_failsafe_ms, self(), {:send_failsafe, ref})

        ui =
          UIServer.replace_ui(
            ui,
            status_ui(" Sending ", ["To: " <> recipient_label(state.recipient), "Sending..."])
          )

        {:noreply, ui, %{state | screen: :sending, pending_ref: ref}}

      {:error, reason} ->
        show_result(ui, state, :failure, "Failed: #{inspect(reason)}")
    end
  end

  def handle_event(:grid, {:clicked, _index, %{id: _id} = _item}, _ui, state) do
    {:noreply, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  defp send_async_to(:broadcast, text), do: MeshtasticCallbacks.send_text_message_async(text)

  defp send_async_to({node_id, _label}, text),
    do: MeshtasticCallbacks.send_direct_message_async(node_id, text)

  defp show_result(ui, state, kind, text) do
    :erlang.send_after(@result_ms, self(), :result_done)

    ui =
      UIServer.replace_ui(
        ui,
        status_ui(" Message status ", ["To: " <> recipient_label(state.recipient), text])
      )

    {:noreply, ui, %{state | screen: :result, result: kind, pending_ref: nil}}
  end

  defp ack_text(:broadcast, _info), do: "Delivered"
  defp ack_text({_node_id, _label}, %{implicit: true}), do: "Sent to mesh"
  defp ack_text({_node_id, _label}, _info), do: "Delivered"

  defp recipient_label(:broadcast), do: "Broadcast"
  defp recipient_label({_node_id, label}), do: label

  defp show_compose(ui, state) do
    new_ui = UIServer.replace_ui(ui, compose_ui(recipient_label(state.recipient)))

    UIServer.begin_widget_state_update(new_ui)
    |> UIServer.update_property!(:message_input, :text, Map.get(state, :draft, ""))
    |> UIServer.apply_widget_state_update(new_ui)
  end

  defp set_recipient_model(ui) do
    UIServer.begin_widget_state_update(ui)
    |> UIServer.update_property!(:recipients, :model, recipient_model())
    |> UIServer.apply_widget_state_update(ui)
  end

  defp recipient_model do
    broadcast = %{
      id: :broadcast,
      text: "Broadcast (LongFast)",
      source: {:pocket_os, "icons/32/generic/mail_doc.rgba"}
    }

    cancel = %{
      id: :cancel,
      text: "Cancel",
      source: {:pocket_os, "icons/32/generic/go_back.rgba"}
    }

    nodes =
      :micronesia.all(:meshtastic_node_info)
      |> Enum.flat_map(fn
        {:meshtastic_node_info, node_id, %{public_key: <<_::binary-32>>} = info} ->
          label = node_label(node_id, info)

          [
            %{
              id: node_id,
              label: label,
              text: label,
              source: {:pocket_os, "icons/32/generic/new_mail.rgba"}
            }
          ]

        _ ->
          []
      end)

    [broadcast, cancel | nodes]
  end

  defp node_label(node_id, info) do
    short = Map.get(info, :short_name)
    id = Map.get(info, :id)

    cond do
      is_binary(short) and is_binary(id) -> "#{short} | #{id}"
      is_binary(id) -> id
      true -> "node #{node_id}"
    end
  end

  defp reload_model(ui, state) do
    inbox_model =
      :micronesia.all(:meshtastic_message)
      |> Enum.map(fn {:meshtastic_message, packet_id, payload} ->
        %{
          id: packet_id,
          text: payload,
          source: {:pocket_os, "icons/32/generic/new_mail.rgba"}
        }
      end)

    exit = %{
      id: :exit,
      text: "Exit",
      source: {:pocket_os, "icons/32/generic/go_back.rgba"}
    }

    compose = %{
      id: :compose,
      text: "Compose",
      source: {:pocket_os, "icons/32/generic/mail_doc.rgba"}
    }

    list_model = [exit, compose | inbox_model]

    updated_ui =
      UIServer.begin_widget_state_update(ui)
      |> UIServer.update_property!(:grid, :model, list_model)
      |> UIServer.apply_widget_state_update(ui)

    {updated_ui, state}
  end
end
