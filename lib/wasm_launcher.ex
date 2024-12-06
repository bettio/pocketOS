alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.TextInputState
alias PhotonUI.UIServer
alias UI.Terminal.TermWidget

defmodule WASMLauncher do
  @compile {:no_warn_undefined, :atomvm}

  def start(file) do
    IO.puts("Going to launch WASM #{file}")

    leader = :erlang.group_leader()

    wbin = :atomvm.read_priv(:pocket_os, file)
    <<header::binary-size(4), _rest::binary>> = wbin

    port = :erlang.open_port({:spawn, "wamr"}, wasm: wbin, start_paused: true)

    :erlang.group_leader(leader, port)
    send(port, :start)
  end
end
