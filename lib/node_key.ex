defmodule NodeKey do
  def load_or_generate(path) do
    case load(path) do
      {:ok, keypair} -> keypair
      _ -> generate_and_persist(path)
    end
  end

  defp load(path) do
    with {:ok, file} <- PocketOS.File.open(path, [:read]),
         {:ok, <<pub::binary-32, priv::binary-32>>} <- PocketOS.File.read(file, 64),
         :ok <- PocketOS.File.close(file) do
      {:ok, {pub, priv}}
    end
  end

  defp generate_and_persist(path) do
    {pub, priv} = :crypto.generate_key(:eddh, :x25519)
    _ = persist(path, <<pub::binary, priv::binary>>)
    {pub, priv}
  end

  defp persist(path, bin) do
    with {:ok, file} <- PocketOS.File.open(path, [:write]),
         {:ok, _} <- PocketOS.File.write(file, bin) do
      PocketOS.File.close(file)
    end
  end
end
