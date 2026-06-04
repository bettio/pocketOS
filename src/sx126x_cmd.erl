%%
%% Copyright (c) 2022 dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(sx126x_cmd).

%%%
%%% @doc
%%% An SPI driver for the LoRa (SX126X) chipset.
%%%
%%% This module can be used to send and receive messages using LoRa modulation.
%%% Currently, this module only supports point-to-point communications.  This
%%% module does not support LoRaWAN.
%%%
%%% References
%%% SemTech SX126x data sheet: https://semtech.my.salesforce.com/sfc/p/#E0000000JelG/a/2R0000001Rbr/6EfVZUorrpoKFfvaF_Fkpgp5kzjiNyiAbqcpqh9qSjE
%%% SemTech reference implementation: https://github.com/Lora-net/sx126x_driver
%%% Python implementation (for interoperability testing): https://github.com/ehong-tl/micropySX126X
%%%
%%% @end

-export([
    set_standby/1, set_standby/2,
    set_standby_xosc/1,
    set_sleep/1, set_sleep/3,
    set_tx/1, set_tx/2,
    set_rx/1, set_rx/2,
    set_regulator_mode/1, set_regulator_mode/2,
    calibration_all/1,
    calibrate_image/2,
    calibrate_image_for_frequency/2,
    apply_tx_clamp_workaround/1,
    set_rx_boosted_gain/2,
    apply_rx_sensitivity_patch/1,
    set_pa_config/2,
    set_rx_tx_fallback_mode/2,
    set_frequency/2,
    set_lora_packet_type/1,
    get_packet_type/1,
    set_tx_params/2, set_tx_params/3,
    set_modulation_params/5,
    set_packet_params/6,
    set_buffer_base_address/3,
    set_sync_word/2,
    set_current_limit/2,
    set_dio2_as_rf_switch_ctl/2,
    set_dio3_as_tcxoc_ctl/1, set_dio3_as_tcxoc_ctl/3,
    disable_all_irq/1,
    set_tx_irq/1,
    set_rx_irq/1,
    set_cad_irq/1,
    set_cad/1,
    get_irq_status/1,
    clear_irq_status/1, clear_irq_status/2,
    get_status/1,
    get_rx_buffer_status/1,
    get_packet_status/1,
    get_device_errors/1,
    clear_device_errors/1,
    write_buffer/2, write_buffer/3,
    read_buffer/3,
    read_register/3,
    write_register/3,
    set_cad_params/1, set_cad_params/6
]).

% -define(TRACE_ENABLED, true).
-include_lib("trace.hrl").

%%
%% SX126x command set
%%
%% The following SPI "commands" (opcodes and arguments) are used to configure
%% the SemTech SX126x modem.
%%
%% See section 13 (commands interface) for the meanings of these SPI commands.
%%

-define(EMPTY_BINARY, <<"">>).
-define(NOP, <<16#00:8>>).

-define(LORA_SYNC_WORD_ADDRESS, 16#0740).

%% @private
set_sync_word(SPI, SyncWord) ->
    ?TRACE("set_sync_word(~p)", [SyncWord]),
    MSB = (SyncWord band 16#F0) bor 16#04,
    LSB = ((SyncWord band 16#0F) bsl 4) bor 16#04,
    Data = <<MSB:8, LSB:8>>,
    _Response = write_register(SPI, ?LORA_SYNC_WORD_ADDRESS, Data),
    ok.

-define(OCP_CURRENT_LIMIT_ADDRESS, 16#08E7).
-define(TX_CLAMP_CONFIG_ADDRESS, 16#08D8).
-define(RX_GAIN_ADDRESS, 16#08AC).
-define(RX_SENSITIVITY_ADDRESS, 16#08B5).

%% @private
set_current_limit(SPI, CurrentLimit) ->
    RawLimit = floor(CurrentLimit / 2.5),
    ?TRACE("set_current_limit(~p, ~p)~n", [CurrentLimit, RawLimit]),
    Data = <<RawLimit:8>>,
    _Response = write_register(SPI, ?OCP_CURRENT_LIMIT_ADDRESS, Data),
    ok.

%% 13.1.1 SetSleep
-define(SET_SLEEP_OPCODE, 16#84).
-define(SLEEP_START_COLD, 2#000).
-define(SLEEP_START_WARM, 2#100).
-define(SLEEP_RTC_DISABLE, 2#000).
-define(SLEEP_RTC_ENABLE, 2#001).

%% @private
set_sleep(SPI) ->
    set_sleep(SPI, ?SLEEP_START_WARM, ?SLEEP_RTC_ENABLE).

%% @private
set_sleep(SPI, Start, RTC) ->
    ?TRACE("SetSleep(~p, ~p)", [Start, RTC]),
    Data = <<(Start bor RTC):8>>,
    write_command(SPI, ?SET_SLEEP_OPCODE, Data).

%% 13.1.2 SetStandby
-define(SET_STANDBY_OPCODE, 16#80).
-define(STDBY_RC, 16#00).
-define(STDBY_XOSC, 16#01).

%% @private
set_standby(SPI) ->
    set_standby(SPI, ?STDBY_RC).

set_standby_xosc(SPI) ->
    set_standby(SPI, ?STDBY_XOSC).

%% @private
set_standby(SPI, StandbyConfig) ->
    ?TRACE("SetStandby(~p)", [StandbyConfig]),
    Data = <<StandbyConfig:8>>,
    write_command(SPI, ?SET_STANDBY_OPCODE, Data).

% %% 13.1.3 SetFS
% -define(SET_FS_OPCODE, 16#C1).

% %% @private
% set_fs(SPI) ->
%     ?TRACE("SetFS()", []),
%     write_command(SPI, ?SET_FS_OPCODE, ?EMPTY_BINARY).

%% 13.1.4 SetTx
-define(SET_TX_OPCODE, 16#83).
-define(TX_TIMEOUT_DISABLE, 16#000000).

%% @private
set_tx(SPI) -> set_tx(SPI, ?TX_TIMEOUT_DISABLE).

%% @private
set_tx(SPI, Timeout) ->
    ?TRACE("SetTx(~p)", [Timeout]),
    Data = <<Timeout:24>>,
    write_command(SPI, ?SET_TX_OPCODE, Data).

%% 13.1.5 SetRx
-define(SET_RX_OPCODE, 16#82).
-define(RX_SINGLE_MODE, 16#000000).
-define(RX_CONTINUOUS_MODE, 16#FFFFFF).

%% @private
set_rx(SPI) ->
    set_rx(SPI, ?RX_CONTINUOUS_MODE).

%% @private
set_rx(SPI, Timeout) ->
    ?TRACE("SetRx(~p)", [Timeout]),
    Data = <<Timeout:24>>,
    write_command(SPI, ?SET_RX_OPCODE, Data).

% %% 13.1.6 StopTimerOnPreamble
% -define(STOP_TIMER_ON_PREAMBLE_OPCODE, 16#9F).
% -define(STOP_TIMER_ON_PREAMBLE_DISABLE, 16#00).
% -define(STOP_TIMER_ON_PREAMBLE_ENABLE,  16#01).

% %% @private
% stop_timer_on_preamble(SPI, Value) ->
%     ?TRACE("StopTimerOnPreamble(~p)", [Value]),
%     Data = <<Value:8>>,
%     write_command(SPI, ?STOP_TIMER_ON_PREAMBLE_OPCODE, Data).

% %% 13.1.7 SetRxDutyCycle
% -define(SET_RX_DUTY_CYCLE_OPCODE, 16#94).

% %% @private
% set_rx_duty_cycle(SPI, RxPeriod, SleepPeriod) ->
%     ?TRACE("SetRxDutyCycle(~p, ~p)", [RxPeriod, SleepPeriod]),
%     Data = <<RxPeriod:24, SleepPeriod:24>>,
%     write_command(SPI, ?SET_RX_DUTY_CYCLE_OPCODE, Data).

%% 13.1.8 SetCad
-define(SET_CAD_OPCODE, 16#C5).

%% @private
set_cad(SPI) ->
    ?TRACE("SetCad()", []),
    write_command(SPI, ?SET_CAD_OPCODE, ?EMPTY_BINARY).

% %% 13.1.9 SetTxContinuousWave
% -define(SET_TX_CONTINUOUS_WAVE_OPCODE, 16#D1).

% %% @private
% set_tx_continuous_wave(SPI) ->
%     ?TRACE("SetTxContinuousWave()", []),
%     write_command(SPI, ?SET_TX_CONTINUOUS_WAVE_OPCODE, ?EMPTY_BINARY).

% %% 13.1.10 SetTxInfinitePreamble
% -define(SET_TX_INFINITE_PREAMBLE_OPCODE, 16#D2).

% %% @private
% set_tx_infinite_preamble(SPI) ->
%     ?TRACE("SetTxInfinitePreamble()", []),
%     write_command(SPI, ?SET_TX_INFINITE_PREAMBLE_OPCODE, ?EMPTY_BINARY).

%% 13.1.11 SetRegulatorMode
-define(SET_REGULATOR_MODE_OPCODE, 16#96).
-define(REGULATOR_MODE_ONLY_LDO, 16#00).
-define(REGULATOR_MODE_DC_DC_LRO, 16#01).

set_regulator_mode(SPI) ->
    set_regulator_mode(SPI, ?REGULATOR_MODE_DC_DC_LRO).

%% @private
set_regulator_mode(SPI, Mode) ->
    ?TRACE("SetRegulatorMode()", []),
    Data = <<Mode:8>>,
    write_command(SPI, ?SET_REGULATOR_MODE_OPCODE, Data).

%% 13.1.12 CalibrateFunction
-define(CALIBRATE_FUNCTION_OPCODE, 16#89).
-define(RC64K_CALIBRATION_ENABLED, 16#01).
-define(RC13M_CALIBRATION_ENABLED, 16#02).
-define(PLL_CALIBRATION_ENABLED, 16#04).
-define(ADC_PULSE_CALIBRATION_ENABLED, 16#08).
-define(ADC_BULK_N_CALIBRATION_ENABLED, 16#10).
-define(ADC_BULK_P_CALIBRATION_ENABLED, 16#20).
-define(IMAGE_CALIBRATION_ENABLED, 16#40).

%% @private
calibration_all(SPI) ->
    calibration_function(SPI, 16#7F).

%% @private
calibration_function(SPI, CalibParam) ->
    ?TRACE("CalibrateFunction(~p)", [CalibParam]),
    Data = <<CalibParam:8>>,
    write_command(SPI, ?CALIBRATE_FUNCTION_OPCODE, Data).

%% 13.1.13 CalibrateImage
-define(CALIBRATE_IMAGE_OPCODE, 16#98).
-define(FREQ_BAND_430_440, <<16#6B:8, 16#6F:8>>).
-define(FREQ_BAND_470_510, <<16#75:8, 16#81:8>>).
-define(FREQ_BAND_779_787, <<16#C1:8, 16#C5:8>>).
-define(FREQ_BAND_863_870, <<16#D7:8, 16#DB:8>>).
-define(FREQ_BAND_902_928, <<16#E1:8, 16#E9:8>>).

calibrate_image(SPI) ->
    calibrate_image(SPI, ?FREQ_BAND_902_928).

%% @private
calibrate_image(SPI, FreqBand) ->
    ?TRACE("CalibrateImage(~p)", [FreqBand]),
    write_command(SPI, ?CALIBRATE_IMAGE_OPCODE, FreqBand).

%% @doc Select the correct image calibration band for the operating frequency.
calibrate_image_for_frequency(SPI, Freq) when
    is_integer(Freq), Freq >= 430000000, Freq =< 440000000
->
    calibrate_image(SPI, ?FREQ_BAND_430_440);
calibrate_image_for_frequency(SPI, Freq) when
    is_integer(Freq), Freq >= 470000000, Freq =< 510000000
->
    calibrate_image(SPI, ?FREQ_BAND_470_510);
calibrate_image_for_frequency(SPI, Freq) when
    is_integer(Freq), Freq >= 779000000, Freq =< 787000000
->
    calibrate_image(SPI, ?FREQ_BAND_779_787);
calibrate_image_for_frequency(SPI, Freq) when
    is_integer(Freq), Freq >= 863000000, Freq =< 870000000
->
    calibrate_image(SPI, ?FREQ_BAND_863_870);
calibrate_image_for_frequency(SPI, Freq) when
    is_integer(Freq), Freq >= 902000000, Freq =< 928000000
->
    calibrate_image(SPI, ?FREQ_BAND_902_928);
calibrate_image_for_frequency(SPI, freq_433mhz) ->
    calibrate_image(SPI, ?FREQ_BAND_430_440);
calibrate_image_for_frequency(SPI, freq_868mhz) ->
    calibrate_image(SPI, ?FREQ_BAND_863_870);
calibrate_image_for_frequency(SPI, freq_915mhz) ->
    calibrate_image(SPI, ?FREQ_BAND_902_928).

apply_tx_clamp_workaround(SPI) ->
    <<Value:8>> = read_register(SPI, ?TX_CLAMP_CONFIG_ADDRESS, 1),
    write_register(SPI, ?TX_CLAMP_CONFIG_ADDRESS, <<(Value bor 16#1E):8>>),
    ok.

set_rx_boosted_gain(SPI, true) ->
    write_register(SPI, ?RX_GAIN_ADDRESS, <<16#96>>),
    ok;
set_rx_boosted_gain(SPI, false) ->
    write_register(SPI, ?RX_GAIN_ADDRESS, <<16#94>>),
    ok.

%% Cleared by calibration_all, must be re-applied after each calibration.
apply_rx_sensitivity_patch(SPI) ->
    <<Value:8>> = read_register(SPI, ?RX_SENSITIVITY_ADDRESS, 1),
    write_register(SPI, ?RX_SENSITIVITY_ADDRESS, <<(Value bor 16#01):8>>),
    ok.

%% 13.1.14 SetPaConfig
-define(SET_PA_CONFIG_OPCODE, 16#95).
%% TODO parameterize -- see datasheet for optimal combinations
-define(PA_DUTY_CYCLE, 16#04).
%% TODO parameterize
-define(HP_MAX, 16#07).
-define(SX1262_SEL, 16#00).
-define(PA_LUT, 16#01).

%% @private
set_pa_config(SPI, sx1262) ->
    set_pa_config(SPI, ?PA_DUTY_CYCLE, ?HP_MAX, ?SX1262_SEL, ?PA_LUT).

%% @private
set_pa_config(SPI, PaDutyCycle, HpMax, DevSel, PaLut) ->
    ?TRACE("SetPaConfig(~p, ~p, ~p, ~p)", [PaDutyCycle, HpMax, DevSel, PaLut]),
    Data = <<PaDutyCycle:8, HpMax:8, DevSel:8, PaLut:8>>,
    write_command(SPI, ?SET_PA_CONFIG_OPCODE, Data).

%% 13.1.15 SetRxTxFallbackMode
-define(SET_RX_TX_FALLBACK_MODE_OPCODE, 16#93).
-define(FALLBACK_MODE_FS, 16#40).
-define(FALLBACK_MODE_XOSC, 16#30).
-define(FALLBACK_MODE_RC, 16#20).

%% @private
set_rx_tx_fallback_mode(SPI, rc) ->
    set_rx_tx_fallback_mode(SPI, ?FALLBACK_MODE_RC);
set_rx_tx_fallback_mode(SPI, xosc) ->
    set_rx_tx_fallback_mode(SPI, ?FALLBACK_MODE_XOSC);
set_rx_tx_fallback_mode(SPI, fs) ->
    set_rx_tx_fallback_mode(SPI, ?FALLBACK_MODE_FS);
set_rx_tx_fallback_mode(SPI, FallbackMode) when is_integer(FallbackMode) ->
    ?TRACE("SetRxTxFallbackMode(~p)", [FallbackMode]),
    Data = <<FallbackMode:8>>,
    write_command(SPI, ?SET_RX_TX_FALLBACK_MODE_OPCODE, Data).

%% 13.2 Registers and Buffer Access

%% 13.2.1 WriteRegister Function
-define(WRITE_REGISTER_OPCODE, 16#0D).

%% @private
write_register(SPI, Address, Data) ->
    ?TRACE("WriteRegister(~p, ~p)", [Address, Data]),
    InputData = <<Address:16, Data/binary>>,
    write_read_command(SPI, ?WRITE_REGISTER_OPCODE, InputData).

%% 13.2.2 ReadRegister Function
-define(READ_REGISTER_OPCODE, 16#1D).

%% @private
read_register(SPI, Address, Len) ->
    ?TRACE("ReadRegister(~p, ~p, ~p)", [SPI, Address, Len]),
    NopPayload = create_nop_payload(Len + 1, []),
    InputData = <<Address:16, NopPayload/binary>>,
    Response = write_read_command(SPI, ?READ_REGISTER_OPCODE, InputData),
    <<_HeaderStatus:3/binary, _NopStatus:8, OutputData/binary>> = Response,
    OutputData.

%% 13.2.3 WriteBuffer Function
-define(WRITE_BUFFER_OPCODE, 16#0E).

%% @private
write_buffer(SPI, Data) ->
    write_buffer(SPI, 0, Data).

%% @private
write_buffer(SPI, Offset, Data) ->
    ?TRACE("WriteBuffer(~p, ~p)", [Offset, Data]),
    InputData = <<Offset:8, Data/binary>>,
    Response = write_read_command(SPI, ?WRITE_BUFFER_OPCODE, InputData),
    Response.

%% 13.2.4 ReadBuffer Function
-define(READ_BUFFER_OPCODE, 16#1E).

%% @private
read_buffer(SPI, Offset, Len) ->
    ?TRACE("ReadBuffer(~p, ~p)", [Offset, Len]),
    NopPayload = create_nop_payload(Len + 1, []),
    InputData = <<Offset:8, NopPayload/binary>>,
    Response = write_read_command(SPI, ?READ_BUFFER_OPCODE, InputData),
    <<_RFU:8, _OffsetStatus:8, _FirstNopStatus:8, OutputData/binary>> = Response,
    OutputData.

%% @private
create_nop_payload(0, Accum) ->
    erlang:iolist_to_binary(Accum);
create_nop_payload(I, Accum) ->
    create_nop_payload(I - 1, [?NOP | Accum]).

%% 13.3 DIO and IRQ Control Functions

%% 13.3.1 SetDioIrqParams
-define(SET_DIO_IRQ_PARAMS_OPCODE, 16#08).
-define(IRQ_MASK_NONE, 2#0000000000).
-define(IRQ_MASK_TX_DONE, 2#0000000001).
-define(IRQ_MASK_RX_DONE, 2#0000000010).
-define(IRQ_MASK_PREABLE_DETECTED, 2#0000000100).
-define(IRQ_MASK_SYNC_WORD_VALID, 2#0000001000).
-define(IRQ_MASK_HEADER_VALID, 2#0000010000).
-define(IRQ_MASK_HEADER_ERR, 2#0000100000).
-define(IRQ_MASK_CRC_ERR, 2#0001000000).
-define(IRQ_MASK_CAD_DONE, 2#0010000000).
-define(IRQ_MASK_CAD_DETECTED, 2#0100000000).
-define(IRQ_MASK_TIMEOUT, 2#1000000000).

-define(IRQ_MASK_LIST, [
    {?IRQ_MASK_TX_DONE, tx_done},
    {?IRQ_MASK_RX_DONE, rx_done},
    {?IRQ_MASK_PREABLE_DETECTED, preamble_detected},
    {?IRQ_MASK_SYNC_WORD_VALID, sync_word_valid},
    {?IRQ_MASK_HEADER_VALID, header_valid},
    {?IRQ_MASK_HEADER_ERR, header_err},
    {?IRQ_MASK_CRC_ERR, crc_err},
    {?IRQ_MASK_CAD_DONE, cad_done},
    {?IRQ_MASK_CAD_DETECTED, cad_detected},
    {?IRQ_MASK_TIMEOUT, timeout}
]).

%% @private
disable_all_irq(SPI) ->
    set_dio_irq_params(SPI, ?IRQ_MASK_NONE, ?IRQ_MASK_NONE, ?IRQ_MASK_NONE, ?IRQ_MASK_NONE).

%% @private
set_tx_irq(SPI) ->
    set_dio_irq_params(
        SPI,
        ?IRQ_MASK_TX_DONE bor ?IRQ_MASK_TIMEOUT,
        ?IRQ_MASK_TX_DONE,
        ?IRQ_MASK_NONE,
        ?IRQ_MASK_NONE
    ).

%% @private
set_rx_irq(SPI) ->
    set_dio_irq_params(
        SPI,
        ?IRQ_MASK_RX_DONE bor ?IRQ_MASK_CRC_ERR bor ?IRQ_MASK_HEADER_ERR bor
            ?IRQ_MASK_TIMEOUT bor ?IRQ_MASK_PREABLE_DETECTED bor ?IRQ_MASK_HEADER_VALID,
        ?IRQ_MASK_RX_DONE,
        ?IRQ_MASK_NONE,
        ?IRQ_MASK_NONE
    ).

%% @private
%% CAD results latch in the status register only; nothing is routed to DIO1,
%% so the caller polls instead of taking an interrupt.
set_cad_irq(SPI) ->
    set_dio_irq_params(
        SPI,
        ?IRQ_MASK_CAD_DONE bor ?IRQ_MASK_CAD_DETECTED,
        ?IRQ_MASK_NONE,
        ?IRQ_MASK_NONE,
        ?IRQ_MASK_NONE
    ).

%% @private
set_dio_irq_params(SPI, IRQMask, DIO1Mask, DIO2Mask, DIO3Mask) ->
    ?TRACE("SetDioIrqParams(~p, ~p, ~p, ~p)", [IRQMask, DIO1Mask, DIO2Mask, DIO3Mask]),
    Data = <<IRQMask:16, DIO1Mask:16, DIO2Mask:16, DIO3Mask:16>>,
    write_command(SPI, ?SET_DIO_IRQ_PARAMS_OPCODE, Data).

%% 13.3.3 GetIrqStatus
-define(GET_IRQ_STATUS_OPCODE, 16#12).

get_irq_status(SPI) ->
    ?TRACE("GetIrqStatus()", []),
    Response = write_read_command(
        SPI, ?GET_IRQ_STATUS_OPCODE, <<?NOP/binary, ?NOP/binary, ?NOP/binary>>
    ),
    % ?TRACE("Response: ~p", [Response]),
    <<_RFU:8, _Status:8, IrqStatus:16>> = Response,
    [Mnemonic || {Mask, Mnemonic} <- ?IRQ_MASK_LIST, Mask band IrqStatus =/= 0].

%% 13.3.4 ClearIrqStatus
-define(CLEAR_IRQ_STATUS_OPCODE, 16#02).

%% @private
clear_irq_status(SPI) ->
    clear_irq_status(SPI, 16#03FF).

clear_irq_status(SPI, Mask) ->
    ?TRACE("ClearIrqStatus(~p)", [Mask]),
    Data = <<Mask:16>>,
    write_command(SPI, ?CLEAR_IRQ_STATUS_OPCODE, Data).

%% 13.3.5 SetDIO2AsRfSwitchCtrl
-define(SET_DIO2_AS_RF_SWITCH_CTL_OPCODE, 16#9D).
-define(DIO2_AS_RF_SWITCH_DISABLE, 16#00).
-define(DIO2_AS_RF_SWITCH_ENABLE, 16#01).

%% @private
set_dio2_as_rf_switch_ctl(SPI, enable) ->
    set_dio2_as_rf_switch_ctl(SPI, ?DIO2_AS_RF_SWITCH_ENABLE);
set_dio2_as_rf_switch_ctl(SPI, disable) ->
    set_dio2_as_rf_switch_ctl(SPI, ?DIO2_AS_RF_SWITCH_DISABLE);
set_dio2_as_rf_switch_ctl(SPI, Enable) ->
    ?TRACE("SetDIO2AsRfSwitchCtrl(~p)", [Enable]),
    Data = <<Enable:8>>,
    write_command(SPI, ?SET_DIO2_AS_RF_SWITCH_CTL_OPCODE, Data).

%% 13.3.6 SetDIO3AsTCXOCtrl
-define(SET_DIO3_AS_TCXOC_CTL_OPCODE, 16#97).
-define(TCXOC_VOLTAGE_16, 16#00).
-define(TCXOC_VOLTAGE_17, 16#01).
-define(TCXOC_VOLTAGE_18, 16#02).
-define(TCXOC_VOLTAGE_22, 16#03).
-define(TCXOC_VOLTAGE_24, 16#04).
-define(TCXOC_VOLTAGE_27, 16#05).
-define(TCXOC_VOLTAGE_30, 16#06).
-define(TCXOC_VOLTAGE_33, 16#07).

%% @private
set_dio3_as_tcxoc_ctl(SPI) ->
    set_dio3_as_tcxoc_ctl(SPI, v_17, 320).

%% @private
set_dio3_as_tcxoc_ctl(SPI, Voltage, Delay) ->
    ?TRACE("SetDIO3AsTCXOCtrl(~p, ~p)", [Voltage, Delay]),
    V = get_voltage(Voltage),
    Data = <<V:8, Delay:24>>,
    write_command(SPI, ?SET_DIO3_AS_TCXOC_CTL_OPCODE, Data).

%% @private
get_voltage(v_16) -> ?TCXOC_VOLTAGE_16;
get_voltage(v_17) -> ?TCXOC_VOLTAGE_17;
get_voltage(v_18) -> ?TCXOC_VOLTAGE_18;
get_voltage(v_22) -> ?TCXOC_VOLTAGE_22;
get_voltage(v_24) -> ?TCXOC_VOLTAGE_24;
get_voltage(v_27) -> ?TCXOC_VOLTAGE_27;
get_voltage(v_30) -> ?TCXOC_VOLTAGE_30;
get_voltage(v_33) -> ?TCXOC_VOLTAGE_33.

%% 13.4 Modulation and Packet-Related Functions

%% 13.4.1 SetRfFrequency
-define(SET_RF_FREQUENCY_OPCODE, 16#86).

%% @private
set_frequency(SPI, freq_169mhz) ->
    % rational:reduce(rational:multiply(169000000, {16384,15625})).
    % {177209344,1}
    set_rf_frequency(SPI, 177209344);
set_frequency(SPI, freq_433mhz) ->
    % rational:reduce(rational:multiply(433000000, {16384,15625})).
    % {454033408,1}
    set_rf_frequency(SPI, 454033408);
set_frequency(SPI, freq_868mhz) ->
    % rational:reduce(rational:multiply(868000000, {16384,15625})).
    % {910163968,1}
    set_rf_frequency(SPI, 910163968);
set_frequency(SPI, freq_915mhz) ->
    % rational:reduce(rational:multiply(915000000, {16384,15625})).
    % {959447040,1}
    set_rf_frequency(SPI, 959447040);
set_frequency(SPI, Freq) when is_integer(Freq) ->
    %% Caution: requires AtomVM fix for parsing external terms > 0x0FFFFFFF
    %% from datasheet
    %%
    %%                  RF     *  F
    %%                    Freq     XTAL
    %% RF           = --------------------
    %%  frequency               25
    %%                         2
    %%
    %% Where F_{XTAL} = 32Mhz
    %%
    {F, _} = rational:simplify(
        rational:reduce(
            rational:multiply(
                Freq,
                %% 2^25/32Mhz or rational:reduce(rational:divide(1 bsl 25, 32000000))
                {16384, 15625}
            )
        )
    ),
    set_rf_frequency(SPI, F).

%% @private
set_rf_frequency(SPI, F) when is_integer(F) ->
    ?TRACE("SetRfFrequency(~p)", [F]),
    % Data = <<F:32>>,
    Data = <<
        ((F bsr 24) band 16#FF):8,
        ((F bsr 16) band 16#FF):8,
        ((F bsr 8) band 16#FF):8,
        (F band 16#FF):8
    >>,
    write_command(SPI, ?SET_RF_FREQUENCY_OPCODE, Data).

%% 13.4.2 SetPacketType
-define(SET_PACKET_TYPE_OPCODE, 16#8A).
-define(PACKET_TYPE_GFSK, 16#00).
-define(PACKET_TYPE_LORA, 16#01).

%% @private
set_lora_packet_type(SPI) -> set_packet_type(SPI, ?PACKET_TYPE_LORA).

%% @private
set_packet_type(SPI, PacketType) ->
    ?TRACE("SetPacketType(~p)", [PacketType]),
    Data = <<PacketType:8>>,
    write_command(SPI, ?SET_PACKET_TYPE_OPCODE, Data).

%% 13.4.3 GetPacketType
-define(GET_PACKET_TYPE_OPCODE, 16#11).

get_packet_type(SPI) ->
    ?TRACE("GetPacketType()", []),
    Data = create_nop_payload(2, []),
    Response = write_read_command(SPI, ?GET_PACKET_TYPE_OPCODE, Data),
    <<_RFU:8, _Status:8, PacketType:8>> = Response,
    PacketType.

% 13.4.4 SetTxParams
-define(SET_TX_PARAMS_OPCODE, 16#8E).
-define(TX_PARAMS_RAMP_10U, 16#00).
-define(TX_PARAMS_RAMP_20U, 16#01).
-define(TX_PARAMS_RAMP_40U, 16#02).
-define(TX_PARAMS_RAMP_80U, 16#03).
-define(TX_PARAMS_RAMP_200U, 16#04).
-define(TX_PARAMS_RAMP_800U, 16#05).
-define(TX_PARAMS_RAMP_1700U, 16#06).
-define(TX_PARAMS_RAMP_3400U, 16#07).

%% @private
set_tx_params(SPI, Power) ->
    set_tx_params(SPI, Power, ?TX_PARAMS_RAMP_200U).

%% @private
set_tx_params(SPI, Power, RampTime) when
    -9 =< Power andalso Power =< 22 andalso 16#00 =< RampTime andalso RampTime =< 16#07
->
    ?TRACE("SetTxParams(~p, ~p)", [Power, RampTime]),
    Data = <<Power:8, RampTime:8>>,
    write_command(SPI, ?SET_TX_PARAMS_OPCODE, Data).

%% 13.4.5 SetModulationParams
-define(SET_MODULATION_PARAMS_OPCODE, 16#8B).

%% @private
set_modulation_params(SPI, SpreadingFactor, BandWidth, CodingRate, LowDataRateOptimize) ->
    SF = sf_value(SpreadingFactor),
    BW = bw_value(BandWidth),
    CR = cr_value(CodingRate),
    LDRO = ldro_value(LowDataRateOptimize),
    ?TRACE("SetModulationParams(~p, ~p, ~p, ~p)", [SF, BW, CR, LDRO]),
    Data = <<SF:8, BW:8, CR:8, LDRO:8>>,
    write_command(SPI, ?SET_MODULATION_PARAMS_OPCODE, Data).

%% @private
sf_value(sf_5) ->
    16#05;
sf_value(sf_6) ->
    16#06;
sf_value(sf_7) ->
    16#07;
sf_value(sf_8) ->
    16#08;
sf_value(sf_9) ->
    16#09;
sf_value(sf_10) ->
    16#0A;
sf_value(sf_11) ->
    16#0B;
sf_value(sf_12) ->
    16#0C;
sf_value(X) when is_integer(X) ->
    io:format(
        "WARNING: Using deprecated spreading factor integer value (~p) -- Use atomic mnemonics, instead.~n",
        [X]
    ),
    X.

%% @private
bw_value(bw_7_8khz) -> 16#00;
bw_value(bw_10_4khz) -> 16#08;
bw_value(bw_15_6khz) -> 16#01;
bw_value(bw_20_8khz) -> 16#09;
bw_value(bw_31_25khz) -> 16#02;
bw_value(bw_41_7khz) -> 16#0A;
bw_value(bw_62_5khz) -> 16#03;
bw_value(bw_125khz) -> 16#04;
bw_value(bw_250khz) -> 16#05;
bw_value(bw_500khz) -> 16#06.

%% @private
cr_value(cr_4_5) -> 16#01;
cr_value(cr_4_6) -> 16#02;
cr_value(cr_4_7) -> 16#03;
cr_value(cr_4_8) -> 16#04.

%% @private
ldro_value(off) -> 16#00;
ldro_value(on) -> 16#01.

%% 13.4.6 SetPacketParams
-define(SET_PACKET_PARAMS_OPCODE, 16#8C).

%% @private
set_packet_params(SPI, PreambleLength, HeaderType, PayloadLength, CRCType, InvertIQ) ->
    HT = ht_value(HeaderType),
    CRC = crc_value(CRCType),
    IIRQ = iirq_value(InvertIQ),
    ?TRACE("SetPacketParams(~p, ~p, ~p, ~p, ~p)", [PreambleLength, HT, PayloadLength, CRC, IIRQ]),
    Data = <<PreambleLength:16, HT:8, PayloadLength:8, CRC:8, IIRQ:8>>,
    write_command(SPI, ?SET_PACKET_PARAMS_OPCODE, Data).

%% @private
ht_value(explicit) -> 16#00;
ht_value(implicit) -> 16#01.

%% @private
crc_value(false) -> 16#00;
crc_value(true) -> 16#01.

%% @private
iirq_value(false) -> 16#00;
iirq_value(true) -> 16#01.

%% 13.4.7 SetCadParams
-define(SET_CAD_PARAMS_OPCODE, 16#88).
-define(CAD_ON_1_SYMB, 16#00).
-define(CAD_ON_2_SYMB, 16#01).
-define(CAD_ON_4_SYMB, 16#02).
-define(CAD_ON_8_SYMB, 16#03).
-define(CAD_ON_16_SYMB, 16#04).

-define(CAD_ONLY, 16#00).
-define(CAD_RX, 16#01).

%% @private
set_cad_params(SPI) ->
    set_cad_params(SPI, 8, 16#19, 10, cad_only, 0).

%% @private
%% NumSymbols is the symbol count (1|2|4|8|16); ExitMode is cad_only | cad_rx.
set_cad_params(SPI, NumSymbols, CadDetPeak, CadDetMin, ExitMode, CadTimeout) ->
    ?TRACE("SetCadParams(~p, ~p, ~p, ~p, ~p)", [
        NumSymbols, CadDetPeak, CadDetMin, ExitMode, CadTimeout
    ]),
    Data =
        <<(cad_symb_value(NumSymbols)):8, CadDetPeak:8, CadDetMin:8, (cad_exit_value(ExitMode)):8,
            CadTimeout:24>>,
    write_command(SPI, ?SET_CAD_PARAMS_OPCODE, Data).

%% @private
cad_symb_value(1) -> ?CAD_ON_1_SYMB;
cad_symb_value(2) -> ?CAD_ON_2_SYMB;
cad_symb_value(4) -> ?CAD_ON_4_SYMB;
cad_symb_value(8) -> ?CAD_ON_8_SYMB;
cad_symb_value(16) -> ?CAD_ON_16_SYMB.

%% @private
cad_exit_value(cad_only) -> ?CAD_ONLY;
cad_exit_value(cad_rx) -> ?CAD_RX.

%% 13.4.8 SetBufferBaseAddress
-define(SET_BUFFER_ADDRESS_OPCODE, 16#8F).

set_buffer_base_address(SPI, TXBaseAddress, RXBaseAddres) ->
    ?TRACE("SetBufferBaseAddress(~p, ~p)", [TXBaseAddress, RXBaseAddres]),
    Data = <<TXBaseAddress:8, RXBaseAddres:8>>,
    write_command(SPI, ?SET_BUFFER_ADDRESS_OPCODE, Data).

%% 13.4.9 SetLoRaSymbNumTimeout
-define(SET_LORA_SYMB_NUM_TIMEOUT_OPCODE, 16#A0).

%% 13.5 Communication Status Information

%% 13.5.1 GetStatus
-define(GET_STATUS_OPCODE, 16#C0).

get_status(SPI) ->
    ?TRACE("GetStatus()", []),
    write_read_command(SPI, ?GET_STATUS_OPCODE, ?NOP).

%% 13.5.2 GetRxBufferStatus
-define(GET_RX_BUFFER_STATUS_OPCODE, 16#13).

get_rx_buffer_status(SPI) ->
    ?TRACE("GetRxBufferStatus()", []),
    Data = create_nop_payload(3, []),
    Response = write_read_command(SPI, ?GET_RX_BUFFER_STATUS_OPCODE, Data),
    <<_RFU:8, _Status:8, PayloadLengthRx:8, RxStartBufferPointer:8>> = Response,
    {PayloadLengthRx, RxStartBufferPointer}.

%% 13.5.3 GetPacketStatus
-define(GET_PACKET_STATUS_OPCODE, 16#14).

get_packet_status(SPI) ->
    ?TRACE("GetPacketStatus()", []),
    Data = create_nop_payload(4, []),
    Response = write_read_command(SPI, ?GET_PACKET_STATUS_OPCODE, Data),
    <<_RFU:8, _Status:8, RssiPkt:8, SnrPkt:8/signed-integer, SignalRssiPkt:8>> = Response,
    {-1 * RssiPkt div 2, SnrPkt div 4, -1 * SignalRssiPkt div 2}.

%% 13.5.4 GetRssiInst
-define(GET_RSSI_INST_OPCODE, 16#15).
%% 13.5.5 GetStats
-define(GET_STATS_OPCODE, 16#10).
%% 13.5.6 ResetStats
-define(RESET_STATS_OPCODE, 16#00).

%% 13.6 Miscellaneous

%% 13.6.1 GetDeviceErrors
-define(GET_DEVICE_ERRORS_OPCODE, 16#17).

get_device_errors(SPI) ->
    ?TRACE("GetDeviceErrors()", []),
    Data = create_nop_payload(3, []),
    Response = write_read_command(SPI, ?GET_DEVICE_ERRORS_OPCODE, Data),
    <<_RFU:8, _Status:8, OpError:16>> = Response,
    OpError.

%% 13.6.2 ClearDeviceErrors
-define(CLEAR_DEVICE_ERRORS_OPCODE, 16#07).

clear_device_errors(SPI) ->
    ?TRACE("ClearDeviceErrors()", []),
    Data = create_nop_payload(2, []),
    Response = write_read_command(SPI, ?CLEAR_DEVICE_ERRORS_OPCODE, Data),
    <<_RFU:8, Status:16>> = Response,
    Status.

%%
%% internal functions
%%

% %% @private
% read_command({SPI, DeviceName}, OpCode) ->
%     {ok, Bin} = spi:read_at(SPI, DeviceName, OpCode, 8),
%     <<Data:8>> = Bin,
%     {ok, Data}.

%% @private
write_command({SPI, DeviceName}, OpCode, Data) ->
    Payload = <<OpCode:8, Data/binary>>,
    % ?TRACE("[erl] write [~s]", [atomvm_lib:to_hex(Payload)]),
    Result = spi:write(SPI, DeviceName, #{write_data => Payload}),
    Result.

%% @private
write_read_command({SPI, DeviceName}, OpCode, Data) ->
    Payload = <<OpCode:8, Data/binary>>,
    {ok, Response} = spi:write_read(SPI, DeviceName, #{write_data => Payload}),
    % ?TRACE("[erl] write-read [~s] -> [~s]", [atomvm_lib:to_hex(Payload), atomvm_lib:to_hex(Response)]),
    Response.
