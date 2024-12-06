defmodule StackedFS do
  @compile {:no_warn_undefined, :atomvm}
  @build_env Mix.env()

  def open(file_server, file_path, mode) do
    :gen_server.call(file_server, {:open, file_path, mode})
  end

  def read({file_server, ref}, bytes) do
    :gen_server.call(file_server, {:read, ref, bytes})
  end

  def close({file_server, ref}) do
    :gen_server.call(file_server, {:close, ref})
  end

  def start_link(base_path) do
    :gen_server.start_link(__MODULE__, [base_path: base_path], [])
  end

  def init(opts) do
    base_path = Keyword.get(opts, :base_path, "./")
    {:ok, %{open_files: %{}, base_path: base_path}}
  end

  def handle_call({:open, file_path, mode}, _from, state) do
    %{open_files: open_files, base_path: base_path} = state

    complete_path = base_path <> file_path

    open_result =
      if @build_env == :test do
        :file.open(complete_path, [:binary | mode])
      else
        case mode do
          [:read] -> :atomvm.posix_open(complete_path, [:o_rdonly])
          [:write] -> :atomvm.posix_open(complete_path, [:o_wronly, :o_creat], 0o644)
          [:read, :write] -> :atomvm.posix_open(complete_path, [:o_rdwr, :o_creat], 0o644)
          [:write, :read] -> :atomvm.posix_open(complete_path, [:o_rdwr, :o_creat], 0o644)
        end
      end

    case open_result do
      {:ok, file} ->
        ref = make_ref()
        updated_state = %{state | open_files: Map.put(open_files, ref, file)}
        {:reply, {:ok, {self(), ref}}, updated_state}

      {:error, _reason} = error ->
        {:reply, error, state}
    end
  end

  def handle_call({:read, ref, bytes}, _from, state) do
    case state do
      %{open_files: %{^ref => file}} ->
        result =
          if @build_env == :test do
            :file.read(file, bytes)
          else
            :atomvm.posix_read(file, bytes)
          end

        {:ok, _data} = result
        {:reply, result, state}

      _not_open ->
        {:reply, {:error, :terminated}, state}
    end
  end

  def handle_call({:write, ref, bytes}, _from, state) do
    case state do
      %{open_files: %{^ref => file}} ->
        result =
          if @build_env == :test do
            :file.write(file, bytes)
          else
            :atomvm.posix_write(file, bytes)
          end

        {:ok, _count} = result
        {:reply, result, state}

      _not_open ->
        {:reply, {:error, :terminated}, state}
    end
  end

  def handle_call({:close, ref}, _from, state) do
    case state do
      %{open_files: %{^ref => file} = open_files} ->
        :ok =
          if @build_env == :test do
            :file.close(file)
          else
            :atomvm.posix_close(file)
          end

        {:reply, :ok, %{state | open_files: Map.delete(open_files, ref)}}

      _ ->
        {:reply, :ok, state}
    end
  end

  def handle_cast(_msg, state) do
    {:reply, :error, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
