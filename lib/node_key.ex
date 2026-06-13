defmodule NodeKey do
  def load_or_generate(
        path,
        keygen \\ fn -> :crypto.generate_key(:eddh, :x25519) end,
        valid? \\ fn _keypair -> true end
      ) do
    case load(path) do
      {:ok, keypair} ->
        if valid?.(keypair) do
          IO.puts("[mesh] NodeKey: loaded existing keypair from #{path}")
          keypair
        else
          IO.puts("[mesh] NodeKey: keypair at #{path} failed validation, generating fresh")
          generate_and_persist(path, keygen)
        end

      error ->
        IO.puts("[mesh] NodeKey: load #{path} failed (#{inspect(error)}), generating fresh")
        generate_and_persist(path, keygen)
    end
  end

  defp load(path) do
    with {:ok, file} <- PocketOS.File.open(path, [:read]),
         read_result <- PocketOS.File.read(file, 64),
         :ok <- PocketOS.File.close(file),
         {:ok, <<pub::binary-32, priv::binary-32>>} <- read_result do
      {:ok, {pub, priv}}
    else
      {:ok, short} when is_binary(short) -> {:error, {:short_read, byte_size(short)}}
      other -> other
    end
  end

  defp generate_and_persist(path, keygen) do
    {pub, priv} = keygen.()
    _ = persist(path, <<pub::binary, priv::binary>>)
    {pub, priv}
  end

  defp persist(path, bin) do
    expected = byte_size(bin)

    with {:ok, file} <- PocketOS.File.open(path, [:read, :write]),
         {:ok, ^expected} <- PocketOS.File.write(file, bin),
         :ok <- PocketOS.File.close(file) do
      :ok
    else
      {:ok, n} when is_integer(n) -> {:error, {:short_write, n, expected}}
      other -> other
    end
  end
end
