defmodule NVSFS do
  # A filesystem over an ESP32 NVS namespace: one blob per file; writes are
  # buffered and flushed to NVS on close.
  @compile {:no_warn_undefined, :esp}
  @build_env Mix.env()

  def open(file_server, file_path, mode) do
    :gen_server.call(file_server, {:open, file_path, mode})
  end

  def read({file_server, ref}, bytes) do
    :gen_server.call(file_server, {:read, ref, bytes})
  end

  def write({file_server, ref}, bin) do
    :gen_server.call(file_server, {:write, ref, bin})
  end

  def close({file_server, ref}) do
    :gen_server.call(file_server, {:close, ref})
  end

  def delete(file_server, file_path) do
    :gen_server.call(file_server, {:delete, file_path})
  end

  def start_link(opts \\ []) do
    :gen_server.start_link(__MODULE__, opts, [])
  end

  def init(opts) do
    namespace = Keyword.get(opts, :namespace, :pocketos)
    {:ok, %{open_files: %{}, namespace: namespace, store: %{}}}
  end

  def handle_call({:open, file_path, mode}, _from, state) do
    with {:ok, {ns, key}} <- path_to_loc(file_path, state.namespace),
         {:ok, handle} <- open_handle(ns, key, mode, state) do
      ref = make_ref()
      state = %{state | open_files: Map.put(state.open_files, ref, handle)}
      {:reply, {:ok, {self(), ref}}, state}
    else
      {:error, _reason} = error -> {:reply, error, state}
    end
  end

  def handle_call({:read, ref, bytes}, _from, state) do
    case state do
      %{open_files: %{^ref => %{pos: pos, buffer: buffer} = handle}} ->
        case byte_size(buffer) - pos do
          left when left <= 0 ->
            {:reply, :eof, state}

          left ->
            count = min(bytes, left)
            <<_::binary-size(pos), chunk::binary-size(count), _::binary>> = buffer
            handle = %{handle | pos: pos + count}
            state = %{state | open_files: Map.put(state.open_files, ref, handle)}
            {:reply, {:ok, chunk}, state}
        end

      _not_open ->
        {:reply, {:error, :terminated}, state}
    end
  end

  def handle_call({:write, ref, bin}, _from, state) do
    case state do
      %{open_files: %{^ref => %{write?: false}}} ->
        {:reply, {:error, :ebadf}, state}

      %{open_files: %{^ref => %{pos: pos, buffer: buffer} = handle}} ->
        <<prefix::binary-size(pos), _::binary>> = buffer
        written_end = pos + byte_size(bin)

        suffix =
          case buffer do
            <<_::binary-size(written_end), tail::binary>> -> tail
            _shorter -> <<>>
          end

        handle = %{handle | pos: written_end, buffer: prefix <> bin <> suffix}
        state = %{state | open_files: Map.put(state.open_files, ref, handle)}
        {:reply, {:ok, byte_size(bin)}, state}

      _not_open ->
        {:reply, {:error, :terminated}, state}
    end
  end

  def handle_call({:close, ref}, _from, state) do
    case state do
      %{open_files: %{^ref => handle} = open_files} ->
        state =
          case handle do
            %{write?: true, namespace: ns, key: key, buffer: buffer} ->
              put_blob(state, ns, key, buffer)

            _ ->
              state
          end

        {:reply, :ok, %{state | open_files: Map.delete(open_files, ref)}}

      _ ->
        {:reply, :ok, state}
    end
  end

  def handle_call({:delete, file_path}, _from, state) do
    case path_to_loc(file_path, state.namespace) do
      {:ok, {ns, key}} -> {:reply, :ok, erase_blob(state, ns, key)}
      {:error, _} = error -> {:reply, error, state}
    end
  end

  def handle_cast(_msg, state) do
    {:reply, :error, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp path_to_loc(file_path, base_ns) do
    name =
      case file_path do
        "/" <> rest -> rest
        _ -> file_path
      end

    case :binary.split(name, "/", [:global]) do
      [key] ->
        with :ok <- valid_seg(key), do: {:ok, {base_ns, :erlang.binary_to_atom(key, :utf8)}}

      [dir, key] ->
        with :ok <- valid_seg(dir),
             :ok <- valid_seg(key) do
          {:ok, {:erlang.binary_to_atom(dir, :utf8), :erlang.binary_to_atom(key, :utf8)}}
        end

      _ ->
        {:error, :enoent}
    end
  end

  defp valid_seg(""), do: {:error, :enoent}
  defp valid_seg(seg) when byte_size(seg) > 15, do: {:error, :enametoolong}
  defp valid_seg(_), do: :ok

  defp open_handle(ns, key, [:read], state) do
    case fetch_blob(state, ns, key) do
      {:ok, blob} -> {:ok, %{namespace: ns, key: key, pos: 0, buffer: blob, write?: false}}
      {:error, :not_found} -> {:error, :enoent}
      {:error, _reason} = error -> error
    end
  end

  defp open_handle(ns, key, [:write], _state) do
    {:ok, %{namespace: ns, key: key, pos: 0, buffer: <<>>, write?: true}}
  end

  defp open_handle(ns, key, mode, state)
       when mode == [:read, :write] or mode == [:write, :read] do
    buffer =
      case fetch_blob(state, ns, key) do
        {:ok, blob} -> blob
        {:error, _} -> <<>>
      end

    {:ok, %{namespace: ns, key: key, pos: 0, buffer: buffer, write?: true}}
  end

  defp fetch_blob(state, ns, key) do
    if @build_env == :test do
      case Map.fetch(state.store, {ns, key}) do
        {:ok, blob} -> {:ok, blob}
        :error -> {:error, :not_found}
      end
    else
      :esp.nvs_fetch_binary(ns, key)
    end
  end

  defp put_blob(state, ns, key, blob) do
    if @build_env == :test do
      %{state | store: Map.put(state.store, {ns, key}, blob)}
    else
      :ok = :esp.nvs_put_binary(ns, key, blob)
      state
    end
  end

  defp erase_blob(state, ns, key) do
    if @build_env == :test do
      %{state | store: Map.delete(state.store, {ns, key})}
    else
      _ = :esp.nvs_erase_key(ns, key)
      state
    end
  end
end
