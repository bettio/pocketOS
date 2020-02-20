defmodule MainMenu do
  
  def start() do
    with {:ok, pid} <- :avm_gen_server.start(__MODULE__, [], []) do
      :avm_gen_server.call(pid, :show)
      {:ok, pid}
    end
  end

  def init(_) do
    {:ok, nil}
  end

  def handle_call(:show, _from, state) do
    icons = [
        {"Test", :icons64.idea_icon()},
        {"Foo", :icons64.warning_icon()},
        {"GBEmu", :icons64.critical_icon()},
        {"Info", :icons64.info_icon()},

        {"Aaa", :icons64.idea_icon()},
        {"Bbb", :icons64.warning_icon()},
        {"Ccc", :icons64.critical_icon()},
        {"Ddd", :icons64.info_icon()},
    ]

    DisplayView.push(IconsMenu, [icons: icons])
    {:reply, :ok, state}
  end

  def handle_call(call, _from, state) do
    :erlang.display(call)

    {:reply, :error, state}
  end

  def handle_info({:icon_clicked, index}, state) do
    :erlang.display({"Icon has been clicked", index})

    {:noreply, state}
  end

  def handle_info(msg, state) do
    :erlang.display(msg)

    {:noreply, state}
  end

  def terminate(_reason, _state) do
    :ok
  end
end
