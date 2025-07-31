defmodule HAL do
  @compile {:no_warn_undefined, :esp}
  @compile {:no_warn_undefined, :gpio}
  @compile {:no_warn_undefined, :spi}

  # @platform "linux"
  # @platform "esp32-devkit"
  # @platform {"m5stack", "faces"}
  # @platform "t-deck"
  @platform "t-pager"

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
    _gpio = :gpio.start()

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
         {:ok, _ref} <- :esp.mount("sdspi", "/sdcard", :fat, spi_host: spi_host, cs: 39),
         {:ok, _} <- FSRegistry.start_link(),
         {:ok, fs0} <- StackedFS.start_link("/sdcard/"),
         :ok <- FSRegistry.register_fs("FS0", fs0) do
      IO.puts("Mounted SD")
    else
      error -> IO.puts("Failed SD mount: #{inspect(error)}")
    end

    ili
  end

  def init("t-pager") do
    _gpio = :gpio.start()

    # AW9364
    backlight_gpio = 42

    :gpio.init(backlight_gpio)
    :gpio.set_pin_mode(backlight_gpio, :output)

    :gpio.digital_write(backlight_gpio, :high)
    :timer.sleep(1)

    Enum.each(0..7, fn _ ->
      :gpio.digital_write(backlight_gpio, :low)
      :gpio.digital_write(backlight_gpio, :high)
    end)

    ili = open_ili9342c_display("t-pager")

    # IO.puts("Mounting SD")
    #
    #    with spi_host when spi_host != :undefined <- :erlang.whereis(:main_spi),
    #         {:ok, _ref} <- :esp.mount("sdspi", "/sdcard", :fat, spi_host: spi_host, cs: 39),
    #         {:ok, _} <- FSRegistry.start_link(),
    #         {:ok, fs0} <- StackedFS.start_link("/sdcard/"),
    #         :ok <- FSRegistry.register_fs("FS0", fs0) do
    #      IO.puts("Mounted SD")
    #    else
    #      error -> IO.puts("Failed SD mount: #{inspect(error)}")
    #    end

    {:ok, pm} = :bq25896_driver.start_link()
    :bq25896_driver.open(pm)

    spawn(fn ->
      IO.puts("Shutdown task started")
      :timer.sleep(10000)
      IO.puts("Shutting down")
      :bq25896_driver.shutdown(pm)
    end)

    {:ok, expio} = :xl9555_driver.start_link()
    :xl9555_driver.open(expio)
    # 3 = lora
    :xl9555_driver.set_direction(expio, 3, :output)
    :xl9555_driver.set_level(expio, 3, :high)

    # 3 = gnss
    :xl9555_driver.set_direction(expio, 4, :output)
    :xl9555_driver.set_level(expio, 4, :high)

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

  defp get_spi_display_opts("t-pager") do
    [
      width: 480,
      height: 222,
      y_offset: 49,
      compatible: "sitronix,st7796",
      cs: 38,
      dc: 37,
      init_list: [
        {0x01, <<0x00>>},
        {:sleep_ms, 120},
        {0x11, <<0x00>>},
        {:sleep_ms, 120},
        {0xF0, <<0xC3>>},
        {0xF0, <<0xC3>>},
        {0xF0, <<0x96>>},
        {0x36, <<0xE8>>},
        {0x3A, <<0x55>>},
        {0xB4, <<0x01>>},
        {0xB6, <<0x80, 0x02, 0x3B>>},
        {0xE8, <<0x40, 0x8A, 0x00, 0x00, 0x29, 0x19, 0xA5, 0x33>>},
        {0xC1, <<0x06>>},
        {0xC2, <<0xA7>>},
        {0xC5, <<0x18>>},
        {:sleep_ms, 120},
        {0xE0,
         <<0xF0, 0x09, 0x0B, 0x06, 0x04, 0x15, 0x2F, 0x54, 0x42, 0x3C, 0x17, 0x14, 0x18, 0x1B>>},
        {0xE1,
         <<0xE0, 0x09, 0x0B, 0x06, 0x04, 0x03, 0x2B, 0x43, 0x42, 0x3B, 0x16, 0x14, 0x17, 0x1B>>},
        {:sleep_ms, 120},
        {0xF0, <<0x3C>>},
        {0xF0, <<0x69>>},
        {:sleep_ms, 120},
        {0x21, <<0x00>>},
        {0x29, <<0x00>>}
      ]
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

  def get_input_devices("t-pager") do
    {:ok, rotary} = :rotary_driver.start_link()

    :rotary_driver.open(rotary, {40, 41, 7})
    :rotary_driver.subscribe(rotary, :all)

    {:ok, keyb} = :tca8418_driver.start_link()
    :ok = :gen_server.call(keyb, :open)
    :ok = :gen_server.call(keyb, {:subscribe_input, :all})

    [keyboard_server: [rotary, keyb]]
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

  defp open_display_spi_host("t-pager") do
    spi_opts = %{
      bus_config: %{sclk: 35, mosi: 34, miso: 33, peripheral: "spi2"},
      device_config: %{
        radio: %{
          clock_speed_hz: 1_000_000,
          mode: 0,
          cs: 36,
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
  defp has_peripheral?("t-pager", "radio"), do: true
  defp has_peripheral?("t-pager", "gps"), do: true
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

  defp get_peripheral_config("t-pager", "radio") do
    case :erlang.whereis(:main_spi) do
      :undefined ->
        :error

      spi ->
        {:ok,
         %{
           radio_module: :lora_sx126x,
           spi: spi,
           device_name: :radio,
           irq: 14,
           reset: 47,
           busy: 48
         }}
    end
  end

  defp get_peripheral_config("t-deck", "gps") do
    {:ok, %{device: "UART1", options: [tx_pin: 43, rx_pin: 44, speed: 38400]}}
  end

  defp get_peripheral_config("t-pager", "gps") do
    # PPS = 13
    {:ok, %{device: "UART1", options: [tx_pin: 12, rx_pin: 4, speed: 38400]}}
  end
end
