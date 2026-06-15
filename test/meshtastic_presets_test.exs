defmodule MeshtasticPresetsTest do
  use ExUnit.Case

  defp errors?(fun) do
    fun.()
    false
  rescue
    _ -> true
  end

  test "EU_868 medium_fast computes 869.525 MHz with both bandwidth keys" do
    p = :meshtastic_presets.preset(:medium_fast, :EU_868)
    assert p.frequency == 869_525_000
    assert p.bandwidth == :bw_250khz
    assert p.bandwidth_hz == 250_000
    assert p.spreading_factor == 9
    assert p.coding_rate == :cr_4_5
    assert p.sync_word == 0x2B
    assert p.channel_name == "MediumFast"
  end

  test "frequency slot computation yields the expected multi-slot frequencies" do
    assert :meshtastic_presets.preset(:long_fast, :EU_433).frequency == 433_875_000
    assert :meshtastic_presets.preset(:long_fast, :US).frequency == 906_875_000
    assert :meshtastic_presets.preset(:long_fast, :EU_868).frequency == 869_525_000
  end

  test "every preset emits both bandwidth representations and the sync word" do
    presets = [
      :long_fast,
      :long_slow,
      :long_moderate,
      :long_turbo,
      :medium_slow,
      :medium_fast,
      :short_slow,
      :short_fast,
      :short_turbo
    ]

    for pr <- presets do
      p = :meshtastic_presets.preset(pr, :US)
      assert is_atom(p.bandwidth)
      assert is_integer(p.bandwidth_hz)
      assert is_integer(p.spreading_factor)
      assert p.sync_word == 0x2B
    end
  end

  test "tx_power defaults to 14, capped to the region legal limit" do
    assert :meshtastic_presets.preset(:long_fast, :EU_868).tx_power == 14
    assert :meshtastic_presets.preset(:long_fast, :US).tx_power == 14
    assert :meshtastic_presets.preset(:long_fast, :EU_433).tx_power == 10
  end

  test "EU_868 rejects the 500 kHz turbo presets, unknowns error" do
    assert errors?(fn -> :meshtastic_presets.preset(:short_turbo, :EU_868) end)
    assert errors?(fn -> :meshtastic_presets.preset(:long_turbo, :EU_868) end)
    assert errors?(fn -> :meshtastic_presets.preset(:bogus, :US) end)
    assert errors?(fn -> :meshtastic_presets.preset(:long_fast, :BOGUS) end)
  end
end
