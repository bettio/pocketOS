defmodule LoraSx126xTest do
  use ExUnit.Case

  # Live preset (EU_433 long-fast)
  @sf11 %{
    spreading_factor: 11,
    bandwidth_hz: 250_000,
    coding_rate: :cr_4_5,
    preamble_length: 16
  }
  @sf9 %{spreading_factor: 9, bandwidth_hz: 250_000, coding_rate: :cr_4_5, preamble_length: 16}
  # LDRO engages (symbol time > 16 ms)
  @sf12_bw125 %{
    spreading_factor: 12,
    bandwidth_hz: 125_000,
    coding_rate: :cr_4_5,
    preamble_length: 16
  }

  describe "channel_windows/1" do
    test "SF11/BW250: 2x preamble airtime and max-frame airtime" do
      assert {331, 2156} = :lora_sx126x.channel_windows(@sf11)
    end

    test "SF9/BW250 scales down" do
      assert {82, 641} = :lora_sx126x.channel_windows(@sf9)
    end

    test "SF12/BW125 takes the LDRO branch" do
      assert {1327, 9281} = :lora_sx126x.channel_windows(@sf12_bw125)
    end

    test "defaults apply when modulation opts are missing" do
      assert {pre, max} = :lora_sx126x.channel_windows(%{})
      assert pre > 0 and max > pre
    end
  end

  describe "classify_channel_activity/4" do
    @windows {331, 2156}

    test "no flags reads free and resets the activity marker" do
      assert {:free, :idle, 0} =
               :lora_sx126x.classify_channel_activity([:tx_done], 5000, 4000, @windows)
    end

    test "first sighting of activity stamps the clock and reads busy" do
      assert {:busy, :activity_detected, 5000} =
               :lora_sx126x.classify_channel_activity([:preamble_detected], 5000, 0, @windows)
    end

    test "preamble within the window stays busy" do
      assert {:busy, :receiving_preamble, 5000} =
               :lora_sx126x.classify_channel_activity([:preamble_detected], 5300, 5000, @windows)
    end

    test "preamble with no header expires as a false detection" do
      assert {:free, :false_preamble, 0} =
               :lora_sx126x.classify_channel_activity([:preamble_detected], 5332, 5000, @windows)
    end

    test "header keeps the channel busy past the preamble window" do
      assert {:busy, :receiving_packet, 5000} =
               :lora_sx126x.classify_channel_activity(
                 [:preamble_detected, :header_valid],
                 6000,
                 5000,
                 @windows
               )
    end

    test "header with no rx_done expires after a max-size frame" do
      assert {:free, :false_header, 0} =
               :lora_sx126x.classify_channel_activity(
                 [:preamble_detected, :header_valid],
                 7157,
                 5000,
                 @windows
               )
    end
  end

  describe "cad_det_peak/1" do
    test "per-SF detection peaks" do
      assert :lora_sx126x.cad_det_peak(5) == 22
      assert :lora_sx126x.cad_det_peak(7) == 22
      assert :lora_sx126x.cad_det_peak(8) == 23
      assert :lora_sx126x.cad_det_peak(9) == 24
      assert :lora_sx126x.cad_det_peak(10) == 25
      assert :lora_sx126x.cad_det_peak(11) == 26
      assert :lora_sx126x.cad_det_peak(12) == 30
    end
  end
end
