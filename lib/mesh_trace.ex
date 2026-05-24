defmodule MeshTrace do
  @mesh_trace System.get_env("MESH_TRACE_ENABLED") != nil

  defmacro trace(msg) do
    if @mesh_trace do
      quote do
        IO.puts(unquote(msg))
      end
    else
      quote do
        _ = fn -> unquote(msg) end
      end
    end
  end
end
