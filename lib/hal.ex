defmodule HAL do
  @compile {:no_warn_undefined, :spi}

  # @platform "linux"
  # @platform "esp32-devkit"
  # @platform {"m5stack", "faces"}
  @platform "t-deck"

  def init() do
    IO.puts("Platform is: #{inspect(@platform)}")
    init(@platform)
  end

  def init("linux") do
    with {:ok, _} <- FSRegistry.start_link(),
         {:ok, fs0} <- StackedFS.start_link("./data/"),
         :ok <- FSRegistry.register_fs("FS0", fs0) do
      IO.puts("Registered fs: ./data as FS0")
    end

    open_sdl_display()
  end

  def init(platform) when platform in [{"m5stack", "faces"}, "esp32-devkit"] do
    open_ili9342c_display(platform)
  end

  def init("t-deck") do
    gpio = :gpio.start()

    board_power_on = 10

    :gpio.init(board_power_on)
    :gpio.set_pin_mode(board_power_on, :output)
    :gpio.digital_write(board_power_on, :high)

    backlight_gpio = 42

    :gpio.init(backlight_gpio)
    :gpio.set_pin_mode(backlight_gpio, :output)

    :gpio.digital_write(backlight_gpio, :high)
    :timer.sleep(1)

    Enum.each(0..7, fn _ ->
      :gpio.digital_write(backlight_gpio, :low)
      :gpio.digital_write(backlight_gpio, :high)
    end)

    ili = open_ili9342c_display("t-deck")

    IO.puts("Mounting SD")

    with spi_host when spi_host != :undefined <- :erlang.whereis(:main_spi),
         {:ok, ref} <- :esp.mount("sdspi", "/sdcard", :fat, spi_host: spi_host, cs: 39),
         {:ok, _} <- FSRegistry.start_link(),
         {:ok, fs0} <- StackedFS.start_link("/sdcard/"),
         :ok <- FSRegistry.register_fs("FS0", fs0) do
      IO.puts("Mounted SD")
    else
      error -> IO.puts("Failed SD mount: #{inspect(error)}")
    end

    ili
  end

  defp open_sdl_display do
    display_opts = [
      width: 320,
      height: 240
    ]

    case :erlang.open_port({:spawn, "display"}, display_opts) do
      display when is_port(display) ->
        {:ok,
         %{
           display: %{
             display_server: {:port, display},
             width: display_opts[:width],
             height: display_opts[:height]
           }
         }}

      _ ->
        IO.puts("Failed to open display")
        :error
    end
  end

  defp get_spi_display_opts("esp32-devkit") do
    [
      width: 320,
      height: 240,
      compatible: "ilitek,ili9341",
      reset: 18,
      cs: 22,
      dc: 21,
      backlight: 5,
      backlight_active: :low,
      backlight_enabled: true,
      rotation: 1,
      enable_tft_invon: false
    ]
  end

  defp get_spi_display_opts({"m5stack", "faces"}) do
    [
      width: 320,
      height: 240,
      compatible: "ilitek,ili9341",
      reset: 33,
      cs: 14,
      dc: 27,
      backlight: 32,
      backlight_enabled: true,
      rotation: 0,
      enable_tft_invon: true
    ]
  end

  defp get_spi_display_opts("t-deck") do
    [
      width: 320,
      height: 240,
      compatible: "sitronix,st7789",
      cs: 12,
      dc: 11,
      rotation: 1,
      enable_tft_invon: true,
      init_seq_type: "alt_gamma_2"
    ]
  end

  def get_input_devices({"m5stack", "faces"}) do
    {:ok, face} = :face.start_link()
    :ok = :gen_server.call(face, :open)
    :ok = :gen_server.call(face, {:subscribe_input, :all})
    [keyboard_server: [face]]
  end

  def get_input_devices("t-deck") do
    # wait the keyboard for 500 ms
    :timer.sleep(500)
    {:ok, face} = :polled_keyboard.start_link()
    :ok = :gen_server.call(face, :open)
    :ok = :gen_server.call(face, {:subscribe_input, :all})

    {:ok, buttons} = :buttons.start_link()

    :ok =
      :gen_server.call(
        buttons,
        {:open, %{0 => :central, 1 => :left, 2 => :right, 3 => :up, 15 => :down}}
      )

    :ok = :gen_server.call(buttons, {:subscribe_input, :all})

    [keyboard_server: [face, buttons]]
  end

  def get_input_devices(_) do
    []
  end

  defp open_display_spi_host("esp32-devkit") do
    spi_opts = %{
      bus_config: %{sclk: 19, mosi: 23, miso: 25, peripheral: "spi2"},
      device_config: %{}
    }

    :spi.open(spi_opts)
  end

  defp open_display_spi_host("t-deck") do
    spi_opts = %{
      bus_config: %{sclk: 40, mosi: 41, miso: 38, peripheral: "spi2"},
      device_config: %{
        radio: %{
          clock_speed_hz: 1_000_000,
          mode: 0,
          cs: 9,
          address_len_bits: 0
        }
      }
    }

    spi = :spi.open(spi_opts)

    true = :erlang.register(:main_spi, spi)

    spi
  end

  defp open_display_spi_host({"m5stack", "faces"}) do
    spi_opts = %{
      bus_config: %{mosi: 23, sclk: 18, peripheral: "spi2"},
      device_config: %{}
    }

    :spi.open(spi_opts)
  end

  defp open_ili9342c_display(platform) do
    spi_host = open_display_spi_host(platform)

    spi_display_opts =
      [spi_host: spi_host] ++ get_spi_display_opts(platform)

    case :erlang.open_port({:spawn, "display"}, spi_display_opts) do
      display when is_port(display) ->
        {:ok,
         %{
           display: %{
             display_server: {:port, display},
             width: spi_display_opts[:width],
             height: spi_display_opts[:height],
             keyboard_server: get_input_devices(platform)[:keyboard_server]
           }
         }}

      _ ->
        IO.puts("Failed to open display")
        :error
    end
  end

  def has_peripheral?(periph) do
    has_peripheral?(@platform, periph)
  end

  defp has_peripheral?("t-deck", "radio"), do: true
  defp has_peripheral?("t-deck", "gps"), do: true
  defp has_peripheral?(_, _), do: false

  def get_peripheral_config(periph) do
    get_peripheral_config(@platform, periph)
  end

  defp get_peripheral_config("t-deck", "radio") do
    case :erlang.whereis(:main_spi) do
      :undefined ->
        :error

      spi ->
        {:ok,
         %{
           radio_module: :lora_sx126x,
           spi: spi,
           device_name: :radio,
           irq: 45,
           reset: 17,
           busy: 13
         }}
    end
  end

  defp get_peripheral_config("t-deck", "gps") do
    {:ok, %{device: "UART1", config: [tx_pin: 43, rx_pin: 44, speed: 38400]}}
  end
end
