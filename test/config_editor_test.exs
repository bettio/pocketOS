defmodule UI.ConfigEditorTest do
  use ExUnit.Case

  defp ensure_registry do
    case FSRegistry.start_link() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end
  end

  test "encode_value round-trips through config_schema validate" do
    assert :config_schema.validate(%{type: :int}, PocketOS.Config.encode_value(:int, 42)) ==
             {:ok, 42}

    assert :config_schema.validate(%{type: :bool}, PocketOS.Config.encode_value(:bool, true)) ==
             {:ok, true}

    assert :config_schema.validate(%{type: :bool}, PocketOS.Config.encode_value(:bool, false)) ==
             {:ok, false}

    assert :config_schema.validate(%{type: :string}, PocketOS.Config.encode_value(:string, "hi")) ==
             {:ok, "hi"}

    e = %{type: :enum, values: [:a, :b]}
    assert :config_schema.validate(e, PocketOS.Config.encode_value(:enum, :b)) == {:ok, :b}
  end

  test "set writes an override and reset clears it back to the default" do
    dir = Path.join(System.tmp_dir!(), "pocketos_edit_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "zz.d"))

    ensure_registry()
    {:ok, fs} = StackedFS.start_link(dir <> "/")
    :ok = FSRegistry.register_fs("FS0", fs)
    :ok = FSRegistry.register_fs("Config", fs)

    schema = %{
      a: %{type: :int, default: 0},
      p: %{type: :enum, values: [:x, :y], default: :x}
    }

    :ok = PocketOS.Config.set(:zz, :a, %{type: :int}, 42)
    :ok = PocketOS.Config.set(:zz, :p, %{type: :enum, values: [:x, :y]}, :y)
    assert {:ok, %{a: 42, p: :y}} = PocketOS.Config.load(:zz, schema: schema)

    :ok = PocketOS.Config.reset(:zz, :a, %{type: :int})
    assert {:ok, %{a: 0, p: :y}} = PocketOS.Config.load(:zz, schema: schema)
  end
end
