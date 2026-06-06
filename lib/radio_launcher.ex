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
        enable_crc: true,
        dio2_as_rf_switch: true,
        tcxo_delay: 320,
        regulator_mode: :dc_dc,
        rx_boosted_gain: true
      }
      |> Map.merge(load_cfg_map("FS0:/radi0cfg.sxp"))

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    complete_config = Map.merge(meshtastic_medium_fast_config, periph_config)
    default_channel_name = "MediumFast"

    {public_key, private_key} = NodeKey.load_or_generate("NVS0:/nodekey.bin")

    id_256 = :crypto.hash(:sha256, public_key)
    <<node_id::little-unsigned-integer-32, _discard::binary>> = id_256
    node_id_string = lpad(Integer.to_string(node_id, 16), 8)
    <<_discard::binary-6, short_node_id::binary-2>> = node_id_string
    <<macaddr::binary-6, _discard::binary>> = id_256
    IO.puts("Node Id is: #{node_id}")

    meshtcfg = load_cfg_map("FS0:/meshtcfg.sxp")

    meshtastic_node_info =
      %{
        user_info: %{
          id: "!#{node_id_string}",
          macaddr: macaddr,
          hw_model: 50,
          role: :CLIENT,
          long_name: "pocketOS #{short_node_id}",
          short_name: short_node_id,
          is_licensed: false,
          public_key: public_key
        }
      }
      |> Map.merge(meshtcfg)

    IO.puts("Node info is: #{inspect(meshtastic_node_info)}")

    channel =
      case Map.get(meshtcfg, :channel_name) do
        nil -> :meshtastic.default_channel(default_channel_name)
        name -> %{name: name, psk: :meshtastic.default_long_fast_psk()}
      end

    {:ok, _rm} =
      :radio_manager.start_link(complete_config, [
        {{:local, :meshcore_server}, :meshcore_server, []},
        {{:local, :meshtastic_server}, :meshtastic_server,
         [
           callbacks: MeshtasticCallbacks,
           node_id: node_id,
           node_info: meshtastic_node_info,
           channel: channel,
           private_key: private_key,
           spreading_factor: Map.fetch!(complete_config, :spreading_factor),
           bandwidth_hz: Map.fetch!(complete_config, :bandwidth_hz),
           coding_rate: Map.fetch!(complete_config, :coding_rate),
           preamble_length: Map.fetch!(complete_config, :preamble_length)
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
