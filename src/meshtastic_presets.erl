-module(meshtastic_presets).

%% Meshtastic radio presets: a {preset, region} pair maps to a full RF config
%% via a modem table, a region table and a frequency-slot computation.

-export([preset/2]).

%% @doc Build the full RF config map for a modem preset in a region.
-spec preset(atom(), atom()) -> map().
preset(P, 'EU_868') when P =:= short_turbo; P =:= long_turbo ->
    error({unsupported_preset, P, 'EU_868'});
preset(Preset, Region) ->
    {Sf, Cr, BwKHz} = modem_params(Preset),
    {FreqStart, FreqEnd, PowerLimit, _Duty} = region_info(Region),
    #{
        frequency => compute_freq(Preset, FreqStart, FreqEnd, BwKHz),
        bandwidth => bw_atom(BwKHz),
        bandwidth_hz => BwKHz * 1000,
        spreading_factor => Sf,
        coding_rate => cr_atom(Cr),
        preamble_length => 16,
        sync_word => 16#2B,
        tx_power => min(14, PowerLimit),
        header_mode => explicit,
        enable_crc => true,
        invert_iq => false,
        dio2_as_rf_switch => true,
        tcxo_delay => 320,
        regulator_mode => dc_dc,
        rx_boosted_gain => true,
        channel_name => list_to_binary(display_name(Preset))
    }.

%% {SpreadingFactor, CodingRateDenominator, BandwidthKHz}
modem_params(long_fast) -> {11, 5, 250};
modem_params(long_slow) -> {12, 8, 125};
modem_params(long_moderate) -> {11, 8, 125};
modem_params(long_turbo) -> {11, 8, 500};
modem_params(medium_slow) -> {10, 5, 250};
modem_params(medium_fast) -> {9, 5, 250};
modem_params(short_slow) -> {8, 5, 250};
modem_params(short_fast) -> {7, 5, 250};
modem_params(short_turbo) -> {7, 5, 500};
modem_params(P) -> error({unknown_preset, P}).

%% Exact preset display-name strings -- the djb2 slot hash depends on them.
display_name(long_fast) -> "LongFast";
display_name(long_slow) -> "LongSlow";
display_name(long_moderate) -> "LongMod";
display_name(long_turbo) -> "LongTurbo";
display_name(medium_slow) -> "MediumSlow";
display_name(medium_fast) -> "MediumFast";
display_name(short_slow) -> "ShortSlow";
display_name(short_fast) -> "ShortFast";
display_name(short_turbo) -> "ShortTurbo".

%% {FreqStartHz, FreqEndHz, PowerLimitDbm, DutyCyclePct}
region_info('US') -> {902000000, 928000000, 30, 100};
region_info('EU_433') -> {433000000, 434000000, 10, 10};
region_info('EU_868') -> {869400000, 869650000, 27, 10};
region_info('CN') -> {470000000, 510000000, 19, 100};
region_info('JP') -> {920500000, 923500000, 13, 100};
region_info('ANZ') -> {915000000, 928000000, 30, 100};
region_info('ANZ_433') -> {433050000, 434790000, 14, 100};
region_info('RU') -> {868700000, 869200000, 20, 100};
region_info('KR') -> {920000000, 923000000, 23, 100};
region_info('TW') -> {920000000, 925000000, 27, 100};
region_info('IN') -> {865000000, 867000000, 30, 100};
region_info('NZ_865') -> {864000000, 868000000, 36, 100};
region_info('TH') -> {920000000, 925000000, 27, 10};
region_info('UA_433') -> {433000000, 434700000, 10, 10};
region_info('UA_868') -> {868000000, 868600000, 14, 1};
region_info('MY_433') -> {433000000, 435000000, 20, 100};
region_info('MY_919') -> {919000000, 924000000, 27, 100};
region_info('SG_923') -> {917000000, 925000000, 20, 100};
region_info('PH_433') -> {433000000, 434700000, 10, 100};
region_info('PH_868') -> {868000000, 869400000, 14, 100};
region_info('PH_915') -> {915000000, 918000000, 24, 100};
region_info('KZ_433') -> {433075000, 434775000, 10, 100};
region_info('KZ_863') -> {863000000, 868000000, 30, 100};
region_info('NP_865') -> {865000000, 868000000, 30, 100};
region_info('BR_902') -> {902000000, 907500000, 30, 100};
region_info('LORA_24') -> {2400000000, 2483500000, 10, 100};
region_info(R) -> error({unknown_region, R}).

compute_freq(Preset, FreqStartHz, FreqEndHz, BwKHz) ->
    BwHz = BwKHz * 1000,
    NumSlots = round((FreqEndHz - FreqStartHz) / BwHz),
    ChannelNum = djb2(display_name(Preset)) rem NumSlots,
    FreqStartHz + BwHz div 2 + ChannelNum * BwHz.

djb2(Str) -> djb2(Str, 5381).

djb2([C | T], H) -> djb2(T, (H * 33 + C) band 16#FFFFFFFF);
djb2([], H) -> H.

bw_atom(125) -> bw_125khz;
bw_atom(250) -> bw_250khz;
bw_atom(500) -> bw_500khz.

cr_atom(5) -> cr_4_5;
cr_atom(6) -> cr_4_6;
cr_atom(7) -> cr_4_7;
cr_atom(8) -> cr_4_8.
