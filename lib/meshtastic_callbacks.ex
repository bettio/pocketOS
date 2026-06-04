defmodule MeshtasticCallbacks do
  require MeshTrace

  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshtastic_message)
    :micronesia.create_table(:meshtastic_position)
    :micronesia.create_table(:meshtastic_node_info)
    # TODO: future evolution: fold signal into the node record
    :micronesia.create_table(:meshtastic_signal)
    :micronesia.create_table(:meshtastic_route)
    :micronesia.create_table(:meshtastic_traceroute)
  end

  def message_cb(
        %{
          message: %{portnum: :TEXT_MESSAGE_APP, payload: payload},
          packet_id: packet_id
        } = msg
      ) do
    pre_process(msg)
    MeshTrace.trace("Got text message: #{inspect(payload)}")
    :micronesia.dirty_write({:meshtastic_message, packet_id, payload})
  end

  def message_cb(
        %{
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
        } = msg
      ) do
    pre_process(msg)
    MeshTrace.trace("Got position message: #{inspect(payload)}")

    :micronesia.dirty_write(
      {:meshtastic_position, src, %{lat: lat * 0.0000001, lon: lon * 0.0000001, alt: alt}}
    )
  end

  def message_cb(
        %{
          message: %{
            portnum: :NODEINFO_APP,
            payload: payload
          },
          src: src
        } = msg
      ) do
    pre_process(msg)
    payload_with_updated = Map.put(payload, :updated_at, :erlang.system_time(:second))

    MeshTrace.trace("Got node info message: #{inspect(payload)}")

    :micronesia.dirty_write({:meshtastic_node_info, src, payload_with_updated})
  end

  def message_cb(
        %{
          message: %{portnum: :TRACEROUTE_APP, payload: route, request_id: request_id},
          src: src
        } = msg
      )
      when is_integer(request_id) and request_id != 0 do
    pre_process(msg)
    MeshTrace.trace("Got traceroute reply from #{inspect(src)}: #{inspect(route)}")

    report =
      Map.merge(route, %{
        reply_rssi: Map.get(msg, :rssi),
        reply_snr: Map.get(msg, :snr),
        received_at: :erlang.system_time(:second)
      })

    :micronesia.dirty_write({:meshtastic_traceroute, src, report})
  end

  def message_cb(
        %{
          message: %{portnum: :TRACEROUTE_APP, payload: route},
          src: src
        } = msg
      ) do
    pre_process(msg)
    MeshTrace.trace("Got traceroute from #{inspect(src)}: #{inspect(route)}")
  end

  def message_cb(msg) do
    pre_process(msg)
    MeshTrace.trace("Got unexpected message: #{inspect(msg)}")
  end

  defp pre_process(%{src: src, rssi: rssi, snr: snr})
       when is_integer(rssi) and is_integer(snr) do
    :micronesia.dirty_write(
      {:meshtastic_signal, src, %{rssi: rssi, snr: snr, last_heard: :erlang.system_time(:second)}}
    )
  end

  defp pre_process(_), do: :ok

  def peer_public_key(node_id) do
    case :micronesia.dirty_read({:meshtastic_node_info, node_id}) do
      [{_, _, %{public_key: <<pub::binary-32>>}}] -> {:ok, pub}
      _ -> {:error, :no_pubkey}
    end
  end

  def learn_route(node_id, next_hop) do
    :micronesia.dirty_write({:meshtastic_route, node_id, next_hop})
  end

  def next_hop_routes(node_ids) do
    Enum.reduce(node_ids, %{}, fn node_id, acc ->
      case :micronesia.dirty_read({:meshtastic_route, node_id}) do
        [{_, _, byte}] -> Map.put(acc, node_id, byte)
        _ -> acc
      end
    end)
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

  def send_direct_message(dest_node_id, text) do
    MeshTrace.trace("[mesh] send_direct_message: dest=#{dest_node_id} #{inspect(text)}")

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send(:meshtastic_server, dest_node_id, data, %{pki: true})
    MeshTrace.trace("[mesh] send_direct_message result: #{inspect(result)}")
    result
  end

  def send_text_message_async(text) do
    MeshTrace.trace("[mesh] send_text_message_async: #{inspect(text)}")

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send_async(:meshtastic_server, 0xFFFFFFFF, data)
    MeshTrace.trace("[mesh] send_text_message_async result: #{inspect(result)}")
    result
  end

  def send_direct_message_async(dest_node_id, text) do
    MeshTrace.trace("[mesh] send_direct_message_async: dest=#{dest_node_id} #{inspect(text)}")

    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send_async(:meshtastic_server, dest_node_id, data, %{pki: true})
    MeshTrace.trace("[mesh] send_direct_message_async result: #{inspect(result)}")
    result
  end

  def send_traceroute(dest_node_id) do
    MeshTrace.trace("[mesh] send_traceroute: dest=#{dest_node_id}")

    data =
      %{portnum: :TRACEROUTE_APP, payload: %{}, want_response: true}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    result = :meshtastic_server.send(:meshtastic_server, dest_node_id, data)
    MeshTrace.trace("[mesh] send_traceroute result: #{inspect(result)}")
    result
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
