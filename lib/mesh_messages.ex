alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.UIServer

defmodule UI.MeshMessages do
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

  @compose_ui [
    %VerticalLayout{
      name: :vl,
      x: 0,
      y: 0,
      width: 320,
      height: 240,
      spacing: 1,
      children: [
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

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def start_monitor(args, opts) do
    UIServer.start_monitor(__MODULE__, args, opts)
  end

  def init(_opts) do
    {:ok, {get_ui(), %{}}, nil}
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
    :micronesia.subscribe({:table, :meshtastic_message, :simple})

    {updated_ui, new_state} = reload_model(ui, state)

    {:noreply, updated_ui, new_state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :exit} = _item}, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(:grid, {:clicked, _index, %{id: :compose} = _item}, ui, state) do
    {:noreply, UIServer.replace_ui(ui, @compose_ui), state}
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
