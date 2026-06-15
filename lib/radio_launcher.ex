defmodule RadioLauncher do
  def start() do
    MeshtasticCallbacks.init()
    MeshcoreCallbacks.init()

    {:ok, radi0cfg} = PocketOS.Config.load(:radi0cfg)
    {:ok, ident} = PocketOS.Config.load(:identity)
    {:ok, mt_cfg} = PocketOS.Config.load(:meshtastic)
    {:ok, mc_cfg} = PocketOS.Config.load(:meshcore)

    preset_rf = preset_config(radi0cfg)

    rf_overrides =
      radi0cfg |> Map.delete(:preset) |> Map.delete(:preset_name) |> Map.delete(:preset_region)

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    complete_config = preset_rf |> Map.merge(rf_overrides) |> Map.merge(periph_config)

    {ed_keypair, {public_key, private_key}} = load_identity_keys()

    id_256 = :crypto.hash(:sha256, public_key)
    <<node_id::little-unsigned-integer-32, _discard::binary>> = id_256
    node_id_string = lpad(Integer.to_string(node_id, 16), 8)
    <<_discard::binary-6, short_node_id::binary-2>> = node_id_string
    <<macaddr::binary-6, _discard::binary>> = id_256
    IO.puts("Node Id is: #{node_id}")

    long_name = Map.get(ident, :long_name, "pocketOS #{short_node_id}")
    short_name = Map.get(ident, :short_name, short_node_id)

    meshtastic_node_info = %{
      user_info: %{
        id: "!#{node_id_string}",
        macaddr: macaddr,
        hw_model: 50,
        role: role_atom(Map.get(mt_cfg, :role, :client)),
        long_name: long_name,
        short_name: short_name,
        is_licensed: false,
        public_key: public_key
      }
    }

    IO.puts("Node info is: #{inspect(meshtastic_node_info)}")

    channel_name = Map.get(mt_cfg, :channel_name, Map.get(preset_rf, :channel_name, "LongFast"))
    channel = :meshtastic.default_channel(channel_name)

    meshcore_identity =
      case ed_keypair do
        {ed_pub, ed_priv} -> [public_key: ed_pub, private_key: ed_priv, name: long_name]
        nil -> []
      end

    meshcore_opts =
      [
        callbacks: MeshcoreCallbacks,
        spreading_factor: Map.fetch!(complete_config, :spreading_factor),
        bandwidth_hz: Map.fetch!(complete_config, :bandwidth_hz),
        coding_rate: Map.fetch!(complete_config, :coding_rate),
        preamble_length: Map.fetch!(complete_config, :preamble_length),
        advert_interval_ms: Map.fetch!(mc_cfg, :advert_interval_s) * 1000,
        answer_discover: Map.fetch!(mc_cfg, :discover_response),
        node_type: Map.fetch!(mc_cfg, :node_type),
        channel_key: mc_channel_key(mc_cfg)
      ] ++ meshcore_identity

    meshtastic_opts = [
      callbacks: MeshtasticCallbacks,
      node_id: node_id,
      node_info: meshtastic_node_info,
      channel: channel,
      private_key: private_key,
      spreading_factor: Map.fetch!(complete_config, :spreading_factor),
      bandwidth_hz: Map.fetch!(complete_config, :bandwidth_hz),
      coding_rate: Map.fetch!(complete_config, :coding_rate),
      preamble_length: Map.fetch!(complete_config, :preamble_length),
      periodic_interval_ms: Map.fetch!(mt_cfg, :node_info_interval_s) * 1000,
      default_hop_limit: Map.fetch!(mt_cfg, :hop_limit),
      node_info_want_response: Map.fetch!(mt_cfg, :want_response_broadcast),
      ok_to_mqtt_bitfield: bool_to_bit(Map.fetch!(mt_cfg, :ok_to_mqtt)),
      enable_relay: Map.fetch!(mt_cfg, :rebroadcast)
    ]

    # One physical radio (one frequency + sync word): the preset picks the band;
    # a stack whose protocol != the preset family is RX-dormant.
    warn_preset_coherence(radi0cfg, mt_cfg, mc_cfg)

    mc_handler = {{:local, :meshcore_server}, :meshcore_server, meshcore_opts}
    mt_handler = {{:local, :meshtastic_server}, :meshtastic_server, meshtastic_opts}

    handlers =
      handler_if(Map.get(mc_cfg, :enabled, false), mc_handler) ++
        handler_if(Map.get(mt_cfg, :enabled, false), mt_handler)

    {:ok, _rm} = :radio_manager.start_link(complete_config, handlers)
  end

  defp load_identity_keys() do
    if :meshcore_protocol.eddsa_available() do
      {ed_pub, ed_priv} =
        NodeKey.load_or_generate(
          "Config:/nodekey.bin",
          fn -> :crypto.generate_key(:eddsa, :ed25519) end,
          &valid_ed25519?/1
        )

      {{ed_pub, ed_priv},
       {:ed25519_x25519.ed_pub_to_x25519(ed_pub), :ed25519_x25519.ed_secret_to_x25519(ed_priv)}}
    else
      {nil, NodeKey.load_or_generate("Config:/nodekey.bin")}
    end
  end

  defp preset_config(cfg) do
    family = Map.get(cfg, :preset, :meshtastic)
    region = Map.get(cfg, :preset_region, :EU_868)
    name = preset_name_atom(cfg)

    try do
      resolve_preset(family, name, region)
    rescue
      e ->
        IO.puts(
          "[radio] bad preset #{inspect({family, name, region})} (#{inspect(e)}); using default"
        )

        resolve_preset(family, nil, region)
    end
  end

  defp preset_name_atom(cfg) do
    case Map.get(cfg, :preset_name) do
      nil -> nil
      name when is_atom(name) -> name
      name when is_binary(name) -> String.to_atom(name)
    end
  end

  defp resolve_preset(:meshcore, name, _region),
    do: :meshcore_presets.preset(name || :eu_original)

  defp resolve_preset(_family, name, region),
    do: :meshtastic_presets.preset(name || :medium_fast, region)

  defp role_atom(:client), do: :CLIENT
  defp role_atom(:client_mute), do: :CLIENT_MUTE
  defp role_atom(:router), do: :ROUTER
  defp role_atom(other), do: other

  defp bool_to_bit(true), do: 1
  defp bool_to_bit(false), do: 0

  defp mc_channel_key(%{channel_key: hex}) when is_binary(hex) and byte_size(hex) > 0 do
    try do
      hex_to_bin(hex)
    rescue
      _ -> :meshcore_protocol.default_public_channel_key()
    end
  end

  defp mc_channel_key(_), do: :meshcore_protocol.default_public_channel_key()

  defp hex_to_bin(hex), do: hex_to_bin(hex, <<>>)
  defp hex_to_bin(<<>>, acc), do: acc

  defp hex_to_bin(<<h, l, rest::binary>>, acc),
    do: hex_to_bin(rest, <<acc::binary, nibble(h) * 16 + nibble(l)>>)

  defp nibble(c) when c in ?0..?9, do: c - ?0
  defp nibble(c) when c in ?a..?f, do: c - ?a + 10
  defp nibble(c) when c in ?A..?F, do: c - ?A + 10

  defp handler_if(true, handler), do: [handler]
  defp handler_if(false, _handler), do: []

  defp warn_preset_coherence(radi0cfg, mt_cfg, mc_cfg) do
    family = Map.get(radi0cfg, :preset, :meshtastic)

    if family == :meshtastic and Map.get(mc_cfg, :enabled, false) do
      IO.puts("[radio] meshcore enabled on a meshtastic RF profile; meshcore RX-dormant")
    end

    if family == :meshcore and Map.get(mt_cfg, :enabled, false) do
      IO.puts("[radio] meshtastic enabled on a meshcore RF profile; meshtastic RX-dormant")
    end

    :ok
  end

  defp valid_ed25519?({pub, priv}) do
    sig = :crypto.sign(:eddsa, :none, "nodekey", [priv, :ed25519])
    :crypto.verify(:eddsa, :none, "nodekey", sig, [pub, :ed25519])
  end

  defp lpad(s, n) when byte_size(s) >= n do
    s
  end

  defp lpad(s, n) do
    lpad("0" <> s, n)
  end
end
