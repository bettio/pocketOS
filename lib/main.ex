defmodule Main do
  @compile {:no_warn_undefined, :alisp}
  @compile {:no_warn_undefined, :ahttp_client}
  @compile {:no_warn_undefined, :avm_pubsub}
  @compile {:no_warn_undefined, :network}
  @compile {:no_warn_undefined, :port}

  def start() do
    :erlang.display("Hello.")

    {:ok, _pubsub} = :avm_pubsub.start(:avm_pubsub)

    with {:ok, initialized} <- HAL.init(),
         %{display: initialized_display} <- initialized,
         %{display_server: display_server, width: width, height: height} <- initialized_display do
      opts = [
        width: width,
        height: height,
        display_server: display_server,
        keyboard_server: initialized_display[:keyboard_server]
      ]

      {:ok, _ui} = UI.start_link(opts, [display_server: display_server] ++ opts)

      if HAL.has_peripheral?("radio") do
        RadioLauncher.start()
      else
        :ok
      end
    else
      _ ->
        IO.puts("Failed HAL init.")
    end

    maybe_start_network()
    maybe_start_init()

    recv_loop()
  end

  defp maybe_start_network() do
    {:ok, net} = PocketOS.Config.load(:network)

    case Map.get(net, :wifi_ssid) do
      nil ->
        :ok

      ssid ->
        creds = [ssid: ssid, psk: Map.get(net, :wifi_psk, "")]
        IO.puts(~s(Will connect to "#{ssid}".))

        case :network.wait_for_sta(creds) do
          :ok ->
            IO.puts("WLAN AP ready. Waiting connections.\n")
            :ok

          {:ok, {_address, _netmask, _gateway} = ips} ->
            IO.puts("Acquired IP address:  #{inspect(ips)}\n")
            :ok

          error ->
            IO.puts("An error occurred starting network: #{inspect(error)}\n")
            :ok
        end
    end
  end

  defp maybe_start_init() do
    with {:ok, file} <- PocketOS.File.open("FS0:/init.lsp", [:read]),
         {:ok, data} <- read_all(file) do
      :alisp.run(data)
      |> :erlang.display()
    end
  end

  defp read_all(file, acc \\ "") do
    case PocketOS.File.read(file, 1024) do
      {:ok, data} when byte_size(data) < 1024 ->
        {:ok, acc <> data}

      {:ok, data} ->
        read_all(file, acc <> data)
    end
  end

  defp recv_loop() do
    receive do
      any -> :erlang.display({:got, any})
    end

    recv_loop()
  end
end
