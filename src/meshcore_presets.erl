-module(meshcore_presets).

%% MeshCore radio presets: named community RF profiles, one fixed frequency per
%% row, with sync 0x12, CR 4/5, and preamble 32 if SF =< 8 else 16.

-export([preset/1]).

%% @doc Build the full RF config map for a named community preset.
-spec preset(atom()) -> map().
preset(eu_original) -> base(869525000, 250, 11, 5);
preset(eu_narrow) -> base(869618000, 62.5, 8, 5);
preset(us_recommended) -> base(910525000, 62.5, 7, 5);
preset(eu433) -> base(433650000, 250, 11, 5);
preset(P) -> error({unknown_preset, P}).

base(FreqHz, BwKHz, Sf, Cr) ->
    #{
        frequency => FreqHz,
        bandwidth => bw_atom(BwKHz),
        bandwidth_hz => round(BwKHz * 1000),
        spreading_factor => Sf,
        coding_rate => cr_atom(Cr),
        preamble_length => preamble(Sf),
        sync_word => 16#12,
        tx_power => 14,
        header_mode => explicit,
        enable_crc => true,
        invert_iq => false,
        dio2_as_rf_switch => true,
        tcxo_delay => 320,
        regulator_mode => dc_dc,
        rx_boosted_gain => true
    }.

preamble(Sf) when Sf =< 8 -> 32;
preamble(_Sf) -> 16.

bw_atom(62.5) -> bw_62_5khz;
bw_atom(125) -> bw_125khz;
bw_atom(250) -> bw_250khz;
bw_atom(500) -> bw_500khz.

cr_atom(5) -> cr_4_5;
cr_atom(6) -> cr_4_6;
cr_atom(7) -> cr_4_7;
cr_atom(8) -> cr_4_8.
