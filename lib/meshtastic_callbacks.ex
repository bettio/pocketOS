defmodule MeshtasticCallbacks do
  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshtastic_message)
    :micronesia.create_table(:meshtastic_position)
  end

  def message_cb(%{message: %{portnum: :TEXT_MESSAGE_APP, payload: payload}, packet_id: packet_id}) do
    IO.puts("Got text message: #{inspect(payload)}")
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
    IO.puts("Got position message: #{inspect(payload)}")

    :micronesia.dirty_write(
      {:meshtastic_position, src, %{lat: lat * 0.0000001, lon: lon * 0.0000001, alt: alt}}
    )
  end

  def message_cb(msg) do
    IO.puts("Got unexpected message: #{inspect(msg)}")
  end

  def send_text_message(text) do
    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    :ok = :meshtastic_server.send(:meshtastic_server, 0xFFFFFFFF, data)
  end

  def send_position(%{lat: lat, lon: lon, alt: alt}) do
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

    :ok = :meshtastic_server.send(:meshtastic_server, 0xFFFFFFFF, data)
  end
end
