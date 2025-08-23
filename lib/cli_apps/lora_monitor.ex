defmodule CLIApps.LoraMonitor do
  def start() do
    lora_config =
      %{
        tx_power: 21,
        frequency: 433_775_000,
        bandwidth: :bw_125khz,
        spreading_factor: 12,
        coding_rate: :cr_4_8,
        preamble_length: 8,
        sync_word: 0x12,
        header_mode: :explicit,
        invert_iq: false,
        enable_crc: true,
        receive_handler: self()
      }

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    %{radio_module: radio_module} = periph_config
    lora_config = Map.merge(lora_config, periph_config)
    {:ok, _lora} = radio_module.start(lora_config)

    recv_loop()
  end

  defp recv_loop() do
    receive do
      msg ->
        inspect(msg)
        |> :erlang.binary_to_list()
        |> IO.puts()
    end

    recv_loop()
  end
end
