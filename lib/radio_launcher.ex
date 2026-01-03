defmodule RadioLauncher do
  def start() do
    MeshtasticCallbacks.init()

    meshtastic_medium_fast_config =
      %{
        tx_power: 14,
        frequency: 869_525_000,
        bandwidth: :bw_250khz,
        bandwidth_hz: 250_000,
        spreading_factor: 9,
        coding_rate: :cr_4_5,
        preamble_length: 16,
        sync_word: 0x2B,
        header_mode: :explicit,
        invert_iq: false,
        enable_crc: false
      }
      |> Map.merge(load_cfg_map("FS0:/radi0cfg.sxp"))

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    complete_config = Map.merge(meshtastic_medium_fast_config, periph_config)

    id_256 = HAL.unique_id_256("meshtastic")
    <<node_id::little-unsigned-integer-32, _discard::binary>> = id_256
    node_id_string = lpad(Integer.to_string(node_id, 16), 8)
    <<_discard::binary-6, short_node_id::binary-2>> = node_id_string
    <<macaddr::binary-6, _discard::binary>> = id_256
    IO.puts("Node Id is: #{node_id}")

    meshtastic_node_info =
      %{
        user_info: %{
          id: "!#{node_id_string}",
          macaddr: macaddr,
          hw_model: 50,
          role: :CLIENT,
          long_name: "pocketOS #{short_node_id}",
          short_name: short_node_id,
          is_licensed: false
        }
      }
      |> Map.merge(load_cfg_map("FS0:/meshtcfg.sxp"))

    IO.puts("Node info is: #{inspect(meshtastic_node_info)}")

    initial_packet_id = :erlang.system_time(:second)
    IO.puts("Initial packet id is: #{initial_packet_id}")

    {:ok, _rm} =
      :radio_manager.start_link(complete_config, [
        {{:local, :meshtastic_server}, :meshtastic_server,
         [
           callbacks: MeshtasticCallbacks,
           node_id: node_id,
           initial_packet_id: initial_packet_id,
           node_info: meshtastic_node_info
         ]}
      ])
  end

  defp lpad(s, n) when byte_size(s) >= n do
    s
  end

  defp lpad(s, n) do
    lpad("0" <> s, n)
  end

  defp load_cfg_map(path) do
    with {:ok, file} <- PocketOS.File.open("FS0:/meshtcfg.sxp", [:read]),
         {:ok, data} <- PocketOS.File.read(file, 8192) do
      data
      |> :sexp_lexer.string()
      |> :sexp_parser.parse()
      |> Enum.map(&List.to_tuple/1)
      |> Enum.into(%{})
    else
      _ -> %{}
    end
  end
end
