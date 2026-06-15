alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconGridView
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.UIServer

defmodule UI.Menu do
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
                width: byte_size(" Menu ") * 8,
                text: " Menu ",
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
          %IconGridView{
            name: :grid,
            x: 0,
            y: 0,
            height: 235,
            width: 320,
            icon_size: 64,
            cell_width: 80,
            cell_height: 100
          }
        ]
      }
    ]
  end

  @menu_model [
    %{
      source: {:pocket_os, "icons/apps/terminal.rgba"},
      text: "ALisp",
      app: UI.Terminal,
      args: [mf: {:arepl, :start}]
    },
    # TODO: add WASM entries
    # WASM example:
    #    %{
    #      source: {:pocket_os, "icons/apps/terminal.rgba"},
    #      text: "WASM",
    #      app: UI.Terminal,
    #      args: [mfa: {WASMLauncher, :start, ["wasi_hello_world.wasm"]}]
    #    },
    %{
      source: {:pocket_os, "icons/apps/terminal.rgba"},
      text: "Lora Mon",
      app: UI.Terminal,
      args: [mf: {CLIApps.LoraMonitor, :start}]
    },
    %{
      source: {:pocket_os, "icons/apps/mail.rgba"},
      text: "MeshMsgs",
      app: UI.MeshMessages,
      args: []
    },
    %{
      source: {:pocket_os, "icons/apps/radio.rgba"},
      text: "MeshNodes",
      app: UI.MeshNodes,
      args: []
    },
    %{
      source: {:pocket_os, "icons/apps/radio.rgba"},
      text: "Traceroute",
      app: UI.Traceroute,
      args: []
    },
    %{
      source: {:pocket_os, "icons/apps/map.rgba"},
      text: "Map",
      app: UI.Map,
      args: []
    },
    %{
      source: {:pocket_os, "icons/apps/radio.rgba"},
      text: "Config",
      app: UI.ConfigEditor,
      args: []
    },
    %{
      source: {:pocket_os, "icons/apps/shut_down.rgba"},
      text: "Shut Down",
      app: UI.ShutDown,
      args: []
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

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, ui, state) do
    updated_ui =
      UIServer.begin_widget_state_update(ui)
      |> UIServer.update_property!(:grid, :model, @menu_model)
      |> UIServer.apply_widget_state_update(ui)

    {:noreply, updated_ui, state}
  end

  def handle_event(:grid, {:clicked, _index, %{app: app, args: args} = _item}, _ui, state) do
    Process.whereis(:ui_server)
    |> send({:show, app, args})

    {:stop, :normal, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end
end
