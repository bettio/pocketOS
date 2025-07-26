alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.Image
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.UIServer

defmodule UI.Map do
  def get_ui(pos, alt, n_sats, display_server) do
    avail_mem =
      try do
        "#{div(:erlang.system_info(:esp32_free_heap_size), 1024)} K"
      rescue
        _ -> "~~~ K"
      end

    {zoom, lat, lon} = pos
    {tile_x, tile_y} = tcoords = tile_coords(zoom, lat, lon)
    {pin_x, pin_y} = pin_offset(tcoords)

    image =
      with {:port, port_driver} <- display_server,
           {:ok, png_data} <- get_tile(zoom, floor(tile_x), floor(tile_y)),
           image_data when is_binary(image_data) <-
             :port.call(port_driver, {:load_image, png_data}) do
        {:rgba8888, 256, 256, image_data}
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
                width: byte_size(" Map ") * 8,
                text: " Map ",
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
          %Container{
            name: :main_area,
            x: 0,
            y: 0,
            width: 320,
            height: 240 - 16,
            children: [
              %Image{
                name: :map_tile,
                x: 320 - 256,
                y:
                  if pin_y + 4 < 240 - 16 do
                    0
                  else
                    240 - 256
                  end,
                width: 256,
                height: 256,
                source: image
              },
              %Rectangle{
                name: :pin_point,
                x: 320 - 256 + pin_x,
                y:
                  if pin_y + 4 < 240 - 16 do
                    pin_y
                  else
                    240 - 256 + pin_y
                  end,
                width: 4,
                height: 4,
                color: 0xFF0000
              },
              %VerticalLayout{
                name: :side_area,
                x: 0,
                y: 0,
                width: 320 - 256,
                height: 240 - 16,
                children: [
                  # %Text{
                  #  name: :sats_label,
                  #  x: 0,
                  #  y: 0,
                  #  text: "Sats:",
                  #  width: 320 - 256,
                  #  height: 16
                  # },
                  # %Text{
                  #  name: :sats_text,
                  #  x: 0,
                  #  y: 0,
                  #  text: "#{n_sats}",
                  #  width: 320 - 256,
                  #  height: 16
                  # },
                  %Text{
                    name: :spacer0,
                    x: 0,
                    y: 0,
                    text: "",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lat_label,
                    x: 0,
                    y: 0,
                    text: "Lat:",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lat_text,
                    x: 0,
                    y: 0,
                    text: truncate(lat, 8),
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lat_ns,
                    x: 0,
                    y: 0,
                    text:
                      if lat >= 0 do
                        "N"
                      else
                        "S"
                      end,
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :spacer1,
                    x: 0,
                    y: 0,
                    text: "",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lon_label,
                    x: 0,
                    y: 0,
                    text: "Lon:",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lon_text,
                    x: 0,
                    y: 0,
                    text: truncate(lon, 8),
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :lon_we,
                    x: 0,
                    y: 0,
                    text:
                      if lon >= 0 do
                        "E"
                      else
                        "W"
                      end,
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :spacer2,
                    x: 0,
                    y: 0,
                    text: "",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :alt_label,
                    x: 0,
                    y: 0,
                    text: "Alt:",
                    width: 320 - 256,
                    height: 16
                  },
                  %Text{
                    name: :alt_text,
                    x: 0,
                    y: 0,
                    text:
                      if alt do
                        "#{truncate(alt * 1.0, 6)} m"
                      else
                        "N/A"
                      end,
                    width: 320 - 256,
                    height: 16
                  },
                  %Button{
                    name: :quit,
                    x: 0,
                    y: 0,
                    height: 32,
                    width: 320 - 256,
                    text: "Quit"
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  end

  defp truncate(num, len) do
    case :erlang.float_to_binary(num, decimals: len + 4) do
      <<truncated::binary-size(len), _rest::binary>> ->
        truncated

      shorter ->
        shorter
    end
  end

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def start_monitor(args, opts) do
    UIServer.start_monitor(__MODULE__, args, opts)
  end

  def init(opts) do
    :avm_pubsub.sub(:avm_pubsub, [:gps])

    if HAL.has_peripheral?("gps") do
      {:ok, gps_config} = HAL.get_peripheral_config("gps")

      {:ok, pid} =
        :gps_server.start(gps_config.device, gps_config.options, fn t ->
          :avm_pubsub.pub(:avm_pubsub, [:gps], t)
        end)
    end

    pos = {0, 0.0, 0.0}
    alt = 0

    display_server = opts[:display_server]

    {:ok, {get_ui(pos, alt, 0, display_server), %{}},
     %{display_server: display_server, has_pos: false}}
  end

  def tile_coords(zoom, lat, lon) do
    map_size = Bitwise.bsl(1, zoom)

    tile_x = (lon + 180) * map_size / 360

    pi = 3.1415926535
    phi = lat
    phi_rad = phi * 2 * pi / 360
    mercator = :math.log((1 + :math.sin(phi_rad)) / (1 - :math.sin(phi_rad))) / 2
    tile_y = (1 - mercator / pi) * (map_size / 2)

    {tile_x, tile_y}
  end

  def pin_offset({tile_x, tile_y}) do
    {round((tile_x - trunc(tile_x)) * 256) - 2, round((tile_y - trunc(tile_y)) * 256) - 2}
  end

  def get_tile(zoom, tile_x, tile_y) do
    case :atomvm.read_priv(:pocket_os, "#{zoom}/#{tile_x}/#{tile_y}.png") do
      bin when is_binary(bin) ->
        {:ok, bin}

      _not_bin ->
        do_get_tile_cache(zoom, tile_x, tile_y)
    end
  end

  def do_get_tile_cache(zoom, tile_x, tile_y) do
    with {:ok, file} <-
           PocketOS.File.open("FS0:/#{fat_file_name(zoom, tile_x, tile_y)}", [:read]),
         {:ok, bin} <- PocketOS.File.read(file, 262_144),
         :ok <- PocketOS.File.close(file) do
      IO.puts("Got file from cache")
      {:ok, bin}
    else
      _ ->
        IO.puts("File not in cache")
        {:ok, bin} = do_get_tile_network(zoom, tile_x, tile_y)
        expected_size = byte_size(bin)

        with {:ok, file} <-
               PocketOS.File.open("FS0:/#{fat_file_name(zoom, tile_x, tile_y)}", [:write]),
             {:ok, ^expected_size} <- PocketOS.File.write(file, bin) do
          IO.puts("Saved file to cache")
          PocketOS.File.close(file)
        end

        {:ok, bin}
    end
  end

  def fat_file_name(zoom, tile_x, tile_y) do
    "#{zoom + tile_x + tile_y}.png"
  end

  def do_get_tile_network(zoom, tile_x, tile_y) do
    {:ok, conn} = :ahttp_client.connect(:http, "tile.openstreetmap.org", 80, active: false)

    {:ok, conn, ref} =
      :ahttp_client.request(
        conn,
        "GET",
        "/#{zoom}/#{tile_x}/#{tile_y}.png",
        [{"User-Agent", "pocketos-map/0.0.1"}],
        :undefined
      )

    recv_tile(conn, <<>>)
  end

  defp recv_tile(conn, received_data) do
    {:ok, updated_conn, responses} = :ahttp_client.recv(conn, 0)

    {continue?, data} =
      Enum.reduce(responses, {true, received_data}, fn
        {:data, _ref, data_chunk}, {true, acc} ->
          {true, acc <> data_chunk}

        {:done, _ref}, {true, acc} ->
          {false, acc}

        _, {true, acc} ->
          {true, acc}
      end)

    if continue? do
      recv_tile(updated_conn, data)
    else
      {:ok, data}
    end
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(
        {:pub, [:gps], _sender,
         %{sentence: sentence, lat: lat, lon: lon, data_status: :valid} = got},
        ui,
        state
      )
      when sentence in [:gll, :rmc] do
    update_ui_lat_lon(lat, lon, nil, nil, ui, state)
  end

  def handle_info(
        {:pub, [:gps], _sender,
         %{sentence: :gga, lat: lat, lon: lon, alt: alt, n_sats: n_sats} = got},
        ui,
        state
      ) do
    update_ui_lat_lon(lat, lon, alt, n_sats, ui, state)
  end

  def handle_info(
        {:pub, [:gps], _sender, %{sentence: :gsv, n_sats: n_sats} = got},
        {wdg, old_state} = ui,
        %{display_server: display_server, has_pos: has_pos} = state
      ) do
    unless has_pos do
      new_ui = get_ui({0, 0.0, 0.0}, 0, n_sats, display_server)
      {:noreply, UIServer.replace_ui({wdg, Map.delete(old_state, :map_tile)}, new_ui), state}
    else
      {:noreply, state}
    end
  end

  def handle_info(msg, ui, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:quit, :clicked, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  def update_ui_lat_lon(lat, lon, alt, n_sats, ui, state) do
    {wdg, old_state} = ui
    %{display_server: display_server} = state

    zoom = 18

    decimal_lat = deg_min_nsew_to_decimal(lat)
    decimal_lon = deg_min_nsew_to_decimal(lon)

    new_ui = get_ui({zoom, decimal_lat, decimal_lon}, alt, n_sats, display_server)

    {:noreply, UIServer.replace_ui({wdg, Map.delete(old_state, :map_tile)}, new_ui),
     Map.put(state, :has_pos, true)}
  end

  def deg_min_nsew_to_decimal(coord) do
    {deg, min, nsew} = coord
    decimal_coord = deg + min / 60

    case nsew do
      :n -> decimal_coord
      :s -> -decimal_coord
      :e -> decimal_coord
      :w -> -decimal_coord
    end
  end
end
