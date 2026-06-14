defmodule PocketOS.File do
  def open(path, mode) do
    [fs_name, fs_path] = :binary.split(path, ":")

    try do
      with {:ok, fs_pid} <- FSRegistry.whereis(fs_name) do
        :gen_server.call(fs_pid, {:open, fs_path, mode})
      else
        {:error, :not_found} -> {:error, :fs_not_available}
      end
    catch
      :exit, {:noproc, {:gen_server, :call, _}} -> {:error, :fs_not_available}
      foo, bar -> IO.puts("Here: #{inspect({foo, bar})}")
    end
  end

  def read({file_server, ref}, count) do
    :gen_server.call(file_server, {:read, ref, count})
  end

  def write({file_server, ref}, bin) do
    :gen_server.call(file_server, {:write, ref, bin})
  end

  def close({file_server, ref}) do
    :gen_server.call(file_server, {:close, ref})
  end

  def delete(path) do
    [fs_name, fs_path] = :binary.split(path, ":")

    try do
      with {:ok, fs_pid} <- FSRegistry.whereis(fs_name) do
        :gen_server.call(fs_pid, {:delete, fs_path})
      else
        {:error, :not_found} -> {:error, :fs_not_available}
      end
    catch
      :exit, {:noproc, {:gen_server, :call, _}} -> {:error, :fs_not_available}
    end
  end
end
