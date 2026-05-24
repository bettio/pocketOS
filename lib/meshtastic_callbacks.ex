defmodule MeshtasticCallbacks do
  require MeshTrace

  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshtastic_message)
    :micronesia.create_table(:meshtastic_position)
    :micronesia.create_table(:meshtastic_node_info)
  end

  def message_cb(%{
        message: %{portnum: :TEXT_MESSAGE_APP, payload: payload},
        packet_id: packet_id
      }) do
    MeshTrace.trace("Got text message: #{inspect(payload)}")
    :micronesia.dirty_write({:meshtastic_message, packet_id, payload})
  end

  def message_cb(%{
        message: %{
          portnum: :POSITION_APP,
          payload:
            %{
              time: _time,
              latitude_i: lat,
              longitude_i: lon,
              altitude: alt
            } = payload
        },
        src: src
      }) do
    MeshTrace.trace("Got position message: #{inspect(payload)}")

    :micronesia.dirty_write(
      {:meshtastic_position, src, %{lat: lat * 0.0000001, lon: lon * 0.0000001, alt: alt}}
    )
  end

  def message_cb(%{
        message: %{
          portnum: :NODEINFO_APP,
          payload: payload
        },
        src: src
      }) do
    payload_with_updated = Map.put(payload, :updated_at, :erlang.system_time(:second))

    MeshTrace.trace("Got node info message: #{inspect(payload)}")

    :micronesia.dirty_write({:meshtastic_node_info, src, payload_with_updated})
  end

  def message_cb(msg) do
    MeshTrace.trace("Got unexpected message: #{inspect(msg)}")
  end

  def peer_public_key(node_id) do
    case :micronesia.dirty_read({:meshtastic_node_info, node_id}) do
      [{_, _, %{public_key: <<pub::binary-32>>}}] -> {:ok, pub}
      _ -> {:error, :no_pubkey}
    end
  end

  def send_text_message(text) do
    MeshTrace.trace("[mesh] send_text_message: #{inspect(text)}")

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send(:meshtastic_server, 0xFFFFFFFF, data)
    MeshTrace.trace("[mesh] send_text_message result: #{inspect(result)}")
    :ok = result
  end

  def send_position(%{lat: lat, lon: lon, alt: alt}) do
    MeshTrace.trace("[mesh] send_position: lat=#{lat} lon=#{lon} alt=#{alt}")
    time = :erlang.system_time(:second)

    position_msg = %{
      portnum: :POSITION_APP,
      payload: %{
        time: time,
        latitude_i: round(lat * 10_000_000),
        longitude_i: round(lon * 10_000_000),
        altitude: round(alt)
      }
    }

    data =
      position_msg
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send(:meshtastic_server, 0xFFFFFFFF, data)
    MeshTrace.trace("[mesh] send_position result: #{inspect(result)}")
    :ok = result
  end
end
