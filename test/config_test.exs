defmodule PocketOS.ConfigTest do
  use ExUnit.Case

  defp ensure_registry do
    case FSRegistry.start_link() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end
  end

  test "loads schema defaults when there are no config files" do
    {:ok, cfg} = PocketOS.Config.load(:radi0cfg)
    assert cfg.preset == :meshtastic
    assert cfg.preset_name == "medium_fast"
    assert cfg.preset_region == :EU_868
    # RF override keys have no default -> omitted, left to the preset
    refute Map.has_key?(cfg, :frequency)
  end

  test "every priv schema parses and yields its defaults" do
    {:ok, mt} = PocketOS.Config.load(:meshtastic)
    assert mt.enabled == false
    assert mt.role == :client
    assert mt.node_info_interval_s == 3600

    {:ok, mc} = PocketOS.Config.load(:meshcore)
    assert mc.enabled == false
    assert mc.node_type == :chat
    assert mc.advert_interval_s == 60

    # identity + network have no defaults -> empty until configured
    assert PocketOS.Config.load(:identity) == {:ok, %{}}
    assert PocketOS.Config.load(:network) == {:ok, %{}}
  end

  test "layers defaults < base < overlay" do
    dir = Path.join(System.tmp_dir!(), "pocketos_cfg_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "xy.d"))
    File.write!(Path.join(dir, "xy.sxp"), ~s|((a 1) (b 2))|)
    File.write!(Path.join(dir, "xy.d/b"), "20")
    File.write!(Path.join(dir, "xy.d/c"), "30")

    ensure_registry()
    {:ok, fs} = StackedFS.start_link(dir <> "/")
    :ok = FSRegistry.register_fs("FS0", fs)
    :ok = FSRegistry.register_fs("Config", fs)

    schema = %{
      a: %{type: :int, default: 0},
      b: %{type: :int, default: 0},
      c: %{type: :int, default: 0},
      d: %{type: :int, default: 99}
    }

    # a: base only; b: overlay over base; c: overlay only; d: schema default
    assert {:ok, %{a: 1, b: 20, c: 30, d: 99}} = PocketOS.Config.load(:xy, schema: schema)
  end
end
