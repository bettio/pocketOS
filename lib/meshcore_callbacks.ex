defmodule MeshcoreCallbacks do
  require MeshTrace

  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshcore_message)
  end

  def message_cb(%{type: :grp_txt, packet_hash: id, text: text}) do
    MeshTrace.trace("Got meshcore group text: #{inspect(text)}")
    :micronesia.dirty_write({:meshcore_message, id, text})
  end

  def message_cb(%{type: :txt_msg, packet_hash: id, text: text} = msg) do
    MeshTrace.trace("Got meshcore direct message: #{inspect(text)}")
    :micronesia.dirty_write({:meshcore_message, id, sender_label(msg) <> ": " <> text})
  end

  def message_cb(_msg), do: :ok

  defp sender_label(%{sender_name: name}) when is_binary(name), do: name
  defp sender_label(%{src_hash: src}), do: "#" <> Integer.to_string(src, 16)
end
