defmodule MeshcorePresetsTest do
  use ExUnit.Case

  defp errors?(fun) do
    fun.()
    false
  rescue
    _ -> true
  end

  test "eu_original community preset" do
    p = :meshcore_presets.preset(:eu_original)
    assert p.frequency == 869_525_000
    assert p.bandwidth == :bw_250khz
    assert p.bandwidth_hz == 250_000
    assert p.spreading_factor == 11
    assert p.coding_rate == :cr_4_5
    assert p.sync_word == 0x12
    assert p.preamble_length == 16
    # MeshCore presets carry no Meshtastic channel name
    refute Map.has_key?(p, :channel_name)
  end

  test "narrow presets use 62.5 kHz and an SF<=8 preamble of 32" do
    p = :meshcore_presets.preset(:eu_narrow)
    assert p.frequency == 869_618_000
    assert p.bandwidth == :bw_62_5khz
    assert p.bandwidth_hz == 62_500
    assert p.spreading_factor == 8
    assert p.preamble_length == 32
    assert p.sync_word == 0x12

    us = :meshcore_presets.preset(:us_recommended)
    assert us.frequency == 910_525_000
    assert us.spreading_factor == 7
    assert us.preamble_length == 32
  end

  test "eu433 on-air-validated preset" do
    p = :meshcore_presets.preset(:eu433)
    assert p.frequency == 433_650_000
    assert p.bandwidth == :bw_250khz
    assert p.spreading_factor == 11
    assert p.preamble_length == 16
  end

  test "unknown meshcore preset errors" do
    assert errors?(fn -> :meshcore_presets.preset(:bogus) end)
  end
end
