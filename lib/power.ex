defmodule PocketOS.Power do
  @compile {:no_warn_undefined, :esp}
  @compile {:no_warn_undefined, :gpio}

  @doc """
  Experimental light sleep: blank the screen and idle the CPU until a key/button
  press or an incoming LoRa packet, then resume the radio.

  No-op when there is no wakeup source (e.g. the host build).
  """
  def light_sleep() do
    case wakeup_sources() do
      [] ->
        :ok

      sources ->
        radio = Process.whereis(:lora_radio)
        if radio, do: safe_radio(:prepare_for_cpu_sleep, radio)
        HAL.set_backlight(:off)

        Enum.each(sources, fn {pin, level} -> :gpio.wakeup_enable(pin, level) end)
        :esp.sleep_enable_gpio_wakeup()
        :esp.light_sleep()

        HAL.set_backlight(:on)
        if radio, do: safe_radio(:resume_after_cpu_sleep, radio)
        :ok
    end
  end

  defp wakeup_sources() do
    HAL.button_wakeup_sources() ++ radio_wakeup_sources()
  end

  defp radio_wakeup_sources() do
    with pid when is_pid(pid) <- Process.whereis(:lora_radio),
         {:ok, %{irq: irq}} <- HAL.get_peripheral_config("radio") do
      [{irq, :high}]
    else
      _ -> []
    end
  end

  # A busy or wedged radio must not crash the sleep path.
  defp safe_radio(fun, radio) do
    apply(:lora_sx126x, fun, [radio])
  catch
    _, _ -> :error
  end
end
