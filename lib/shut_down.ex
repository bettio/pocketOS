alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.UIServer

defmodule UI.ShutDown do
  def get_ui(_display_server) do
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
                width: byte_size(" Shut Down ") * 8,
                text: " Shut Down ",
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
          %Button{
            name: :power_off,
            x: 0,
            y: 0,
            height: 32,
            width: 240,
            text: "Power Off"
          },
          %Button{
            name: :cancel,
            x: 0,
            y: 48,
            height: 32,
            width: 240,
            text: "Cancel"
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

  def init(opts) do
    display_server = opts[:display_server]

    {:ok, {get_ui(display_server), %{}}, %{display_server: display_server, has_pos: false}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(msg, _ui, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:power_off, :clicked, _ui, state) do
    with pm when pm != :undefined <- :erlang.whereis(:pm) do
      IO.puts("Shutdown started")
      :bq25896_driver.shutdown(pm)
    else
      _error ->
        IO.puts("No power management available")
    end

    {:noreply, state}
  end

  def handle_event(:cancel, :clicked, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end
end
