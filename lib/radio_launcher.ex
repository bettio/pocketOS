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

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    complete_config = Map.merge(meshtastic_medium_fast_config, periph_config)

    <<node_id::little-unsigned-integer-32, _discard::binary>> = HAL.unique_id_256("meshtastic")
    IO.puts("Node Id is: #{node_id}")

    initial_packet_id = :erlang.system_time(:second)
    IO.puts("Initial packet id is: #{initial_packet_id}")

    {:ok, _rm} =
      :radio_manager.start_link(complete_config, [
        {{:local, :meshtastic_server}, :meshtastic_server,
         [callbacks: MeshtasticCallbacks, node_id: node_id, initial_packet_id: initial_packet_id]}
      ])
  end
end
