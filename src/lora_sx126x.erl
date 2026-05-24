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
-module(lora_sx126x).

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

%% Internal Lora provider API
-export([start/1, start_link/1, stop/1, broadcast/2, sleep/1]).

%% gen_statem
-export([init/1, waiting_to_receive/3, waiting_tx_done/3, terminate/3]).

% -define(TRACE_ENABLED, true).
-include_lib("trace.hrl").

-define(MAX_PACKET_LEN, 255).
-define(AGC_RESET_INTERVAL_MS, 60000).
%% 10 seconds in 15.625us ticks - safe for all SF/BW combos
-define(TX_HW_TIMEOUT, 640000).

%%
%% Lora Provider API
%%

%% @hidden
start(Config) ->
    gen_statem:start(?MODULE, Config, []).

%% @hidden
start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

%% @hidden
stop(Lora) ->
    gen_statem:stop(Lora).

%% @hidden
broadcast(Lora, Message) ->
    gen_statem:call(Lora, {broadcast, Message}).

%% @hidden
sleep(Lora) ->
    gen_statem:call(Lora, sleep).

%%%
%%% gen_statem implementation
%%%

-record(state, {
    spi,
    config,
    irq,
    busy_pin,
    pending
}).

%% @hidden
init(Config) ->
    ?TRACE("init(~p)", [Config]),
    SPI = {maps:get(spi, Config), maps:get(device_name, Config)},

    ok = maybe_reset(maps:get(reset, Config, undefined)),

    BusyPin = maps:get(busy, Config, undefined),
    gpio:set_pin_mode(BusyPin, input),
    case maybe_wait_until_not_busy(BusyPin, 1000) of
        ok ->
            case init_lora(SPI, Config) of
                ok ->
                    GPIO = gpio:start(),
                    ok = maybe_set_interrupt(GPIO, rising, maps:get(irq, Config, undefined)),

                    set_recv_mode_full(SPI, Config),

                    erlang:send_after(?AGC_RESET_INTERVAL_MS, self(), agc_reset),

                    State = #state{
                        spi = SPI,
                        config = Config,
                        irq = maps:get(irq, Config, undefined),
                        busy_pin = BusyPin
                    },
                    {ok, waiting_to_receive, State};
                LoraError ->
                    {stop, LoraError}
            end;
        Error ->
            {stop, Error}
    end.

%%
%% gen_statem state machine functions
%%

%%
%% waiting_to_receive state
%%  We are waiting to receive messages
%%  * If we get a request to broadcast a message, we broadcast the message
%%    and go into the waiting_tx_done state, to wait for the TX_DONE IRQ
%%  * Any unknown calls are rejected with an error
%%  * Any unknown messages are silently discarded
%%

%% @hidden
waiting_to_receive(info, {gpio_interrupt, Pin}, #state{irq = Pin} = State) ->
    ?TRACE("gpio_interrupt IRQ Pin=~p Level=~p in waiting_to_receive state", [
        Pin, gpio:digital_read(Pin)
    ]),
    do_receive(State),
    {next_state, waiting_to_receive, State};
waiting_to_receive(info, agc_reset, State) ->
    do_agc_reset(State),
    erlang:send_after(?AGC_RESET_INTERVAL_MS, self(), agc_reset),
    {next_state, waiting_to_receive, State};
%% TODO: decide whether channel-busy retry belongs here or on the caller side
waiting_to_receive({call, From}, {broadcast, Message}, State) ->
    case wait_channel_free(State#state.spi, State#state.busy_pin, 10) of
        ok ->
            ok = do_broadcast(State#state.spi, State#state.config, Message),
            {next_state, waiting_tx_done, State#state{pending = From}, [
                {state_timeout, 9000, {error, tx_timeout}}
            ]};
        Error ->
            {next_state, waiting_to_receive, State, [{reply, From, Error}]}
    end;
waiting_to_receive({call, From}, sleep, State) ->
    do_sleep(State#state.spi),
    {next_state, sleep, State, [{reply, From, ok}]};
waiting_to_receive({call, From}, Request, State) ->
    ?TRACE("Unhandled call in waiting_to_receive state.  Request: ~p", [Request]),
    {next_state, waiting_to_receive, State, [{reply, From, {error, unknown_request}}]};
waiting_to_receive(EventType, Request, State) ->
    ?TRACE("Unhandled message in waiting_to_receive state.  EventType: ~p Request: ~p", [
        EventType, Request
    ]),
    {next_state, waiting_to_receive, State}.

%%
%% waiting_tx_done state
%%  We are waiting for a signal that transmission of a message has completed
%%  * If we get the expected interrupt on the IRQ pin, then reply ok to caller
%%    and go back into the waiting_to_receive state
%%  * If we timeout waiting, then reply to caller with a timeout error
%%  * Any calls are rejected with an error
%%  * Any unknown messages are silently discarded
%%

waiting_tx_done(info, {gpio_interrupt, Pin}, #state{irq = Pin} = State) ->
    ?TRACE("gpio_interrupt IRQ Pin=~p Level=~p in waiting_tx_done state~n", [
        Pin, gpio:digital_read(Pin)
    ]),
    case maybe_wait_until_not_busy(State#state.busy_pin, 1000) of
        ok ->
            set_recv_mode(State#state.spi, State#state.config);
        _E ->
            ?TRACE("Error!  Unable to get into receive mode!~n", [])
    end,
    NewState = State#state{pending = undefined},
    {next_state, waiting_to_receive, NewState, [{reply, State#state.pending, ok}]};
waiting_tx_done(info, agc_reset, _State) ->
    erlang:send_after(?AGC_RESET_INTERVAL_MS, self(), agc_reset),
    {keep_state_and_data, []};
waiting_tx_done(state_timeout, ErrorMessage, State) ->
    ?TRACE("Timed out waiting for tx_done IRQ.  Error message: ~p~n", [ErrorMessage]),
    set_recv_mode(State#state.spi, State#state.config),
    NewState = State#state{pending = undefined},
    ?TRACE("going back into receive state.  Will reply with ErrorMessage=~p", [ErrorMessage]),
    {next_state, waiting_to_receive, NewState, [{reply, State#state.pending, ErrorMessage}]};
waiting_tx_done({call, From}, Request, State) ->
    ?TRACE("Illegal call in waiting_tx_done state.  Request: ~p~n", [Request]),
    {next_state, waiting_tx_done, State, [{reply, From, {error, busy_waiting_tx_done}}]};
waiting_tx_done(EventType, Request, State) ->
    ?TRACE("Unhandled message in waiting_tx_done state.  EventType: ~p Request: ~p~n", [
        EventType, Request
    ]),
    {next_state, waiting_tx_done, State}.

%% @hidden
terminate(_Reason, _CurrentState, _State) ->
    ok.

%%%
%%% internal functions
%%%

%% @private
init_lora(SPI, Config) ->
    ok = sx126x_cmd:set_standby(SPI),

    %% SetPacketType must be first config command per datasheet
    ok = sx126x_cmd:set_lora_packet_type(SPI),
    ?TRACE("packet type: ~p~n", [sx126x_cmd:get_packet_type(SPI)]),

    %% TCXO must be enabled before calibration so it runs against stable clock
    TcxoVoltage = maps:get(tcxo_voltage, Config, v_18),
    TcxoDelay = maps:get(tcxo_delay, Config, 320),
    ok = sx126x_cmd:set_dio3_as_tcxoc_ctl(SPI, TcxoVoltage, TcxoDelay),

    ok = sx126x_cmd:calibration_all(SPI),

    Frequency = maps:get(frequency, Config, freq_915mhz),
    ok = sx126x_cmd:calibrate_image_for_frequency(SPI, Frequency),

    case maps:get(regulator_mode, Config, dc_dc) of
        dc_dc -> ok = sx126x_cmd:set_regulator_mode(SPI);
        ldo -> ok
    end,

    case maps:get(dio2_as_rf_switch, Config, true) of
        true -> ok = sx126x_cmd:set_dio2_as_rf_switch_ctl(SPI, enable);
        false -> ok
    end,

    ok = sx126x_cmd:set_frequency(SPI, Frequency),
    ok = sx126x_cmd:set_pa_config(SPI, sx1262),
    ok = sx126x_cmd:set_tx_params(SPI, maps:get(tx_power, Config, 2)),
    ok = sx126x_cmd:apply_tx_clamp_workaround(SPI),
    ok = sx126x_cmd:set_current_limit(SPI, 140),

    Ldro = compute_ldro(Config),
    ok = sx126x_cmd:set_modulation_params(
        SPI,
        maps:get(spreading_factor, Config, sf_7),
        maps:get(bandwidth, Config, bw_125khz),
        maps:get(coding_rate, Config, cr_4_8),
        Ldro
    ),

    ok = sx126x_cmd:set_packet_params(
        SPI,
        maps:get(preamble_length, Config, 8),
        maps:get(header_mode, Config, explicit),
        maps:get(max_payload_length, Config, 16#FF),
        maps:get(enable_crc, Config, true),
        maps:get(invert_iq, Config, false)
    ),

    ok = sx126x_cmd:set_sync_word(SPI, maps:get(sync_word, Config, 16#12)),
    ok = sx126x_cmd:set_rx_tx_fallback_mode(SPI, xosc),

    ok = sx126x_cmd:set_buffer_base_address(
        SPI,
        maps:get(tx_base_address, Config, 16#00),
        maps:get(rx_base_address, Config, 16#00)
    ),

    ok = sx126x_cmd:set_rx_boosted_gain(SPI, maps:get(rx_boosted_gain, Config, true)),

    %% Must be after calibration_all since cal clears it
    ok = sx126x_cmd:apply_rx_sensitivity_patch(SPI),

    _Status = sx126x_cmd:clear_device_errors(SPI),
    ok = sx126x_cmd:clear_irq_status(SPI),

    ?TRACE("device_errors: ~p", [sx126x_cmd:get_device_errors(SPI)]),
    ok.

%% @private
%% Full RX setup: called from init and after AGC reset.
set_recv_mode_full(SPI, Config) ->
    ?TRACE("Setting mode to recv (full)", []),
    ok = sx126x_cmd:set_standby(SPI),
    ok = sx126x_cmd:set_rx_irq(SPI),
    ok = sx126x_cmd:set_buffer_base_address(
        SPI,
        maps:get(tx_base_address, Config, 16#00),
        maps:get(rx_base_address, Config, 16#00)
    ),
    ok = sx126x_cmd:clear_irq_status(SPI),
    ok = sx126x_cmd:set_packet_params(
        SPI,
        maps:get(preamble_length, Config, 8),
        maps:get(header_mode, Config, explicit),
        maps:get(max_payload_length, Config, 16#FF),
        maps:get(enable_crc, Config, true),
        maps:get(invert_iq, Config, false)
    ),
    ok = sx126x_cmd:set_rx(SPI),
    ok.

%% @private
%% Slim RX re-entry: called after each RX/TX cycle.
set_recv_mode(SPI, _Config) ->
    ?TRACE("Setting mode to recv", []),
    ok = sx126x_cmd:set_standby(SPI),
    ok = sx126x_cmd:clear_irq_status(SPI),
    ok = sx126x_cmd:set_rx_irq(SPI),
    ok = sx126x_cmd:set_rx(SPI),
    ok.

%% @private
maybe_set_interrupt(_GPIO, _Trigger, undefined) ->
    ok;
maybe_set_interrupt(GPIO, Trigger, Pin) ->
    ?TRACE("maybe_set_interrupt on pin ~p for trigger ~p~n", [Pin, Trigger]),
    gpio:set_pin_pull(Pin, down),
    gpio:set_int(GPIO, Pin, Trigger).

%% @private
maybe_reset(undefined) ->
    ?TRACE("Reset pin not set.  Skipping...", []),
    ok;
maybe_reset(ResetPin) ->
    ?TRACE("Resetting on pin ~p ...", [ResetPin]),
    ok = gpio:set_pin_mode(ResetPin, output),
    ok = gpio:digital_write(ResetPin, high),
    timer:sleep(1),
    ok = gpio:digital_write(ResetPin, low),
    timer:sleep(1),
    ok = gpio:digital_write(ResetPin, high),
    ok.

%% @private
maybe_wait_until_not_busy(undefined, _I) ->
    ok;
maybe_wait_until_not_busy(_BusyPin, 0) ->
    {error, timeout_busy};
maybe_wait_until_not_busy(BusyPin, I) ->
    case gpio:digital_read(BusyPin) of
        low ->
            ?TRACE("Pin ~p is not busy.", [BusyPin]),
            ok;
        _ ->
            ?TRACE("Pin ~p still busy.  Waiting some more.", [BusyPin]),
            timer:sleep(1),
            maybe_wait_until_not_busy(BusyPin, I - 1)
    end.

%%%
%%% send
%%%

do_broadcast(SPI, Config, Data) ->
    Len = erlang:byte_size(Data),
    case Len > ?MAX_PACKET_LEN of
        true ->
            {error, payload_too_large};
        _ ->
            ?TRACE("preparing transmit...~n", []),

            ok = sx126x_cmd:set_standby_xosc(SPI),

            ok = sx126x_cmd:set_packet_params(
                SPI,
                maps:get(preamble_length, Config, 8),
                maps:get(header_mode, Config, explicit),
                Len,
                maps:get(enable_crc, Config, true),
                maps:get(invert_iq, Config, false)
            ),

            ok = sx126x_cmd:clear_irq_status(SPI),
            ok = sx126x_cmd:set_tx_irq(SPI),

            ok = sx126x_cmd:set_buffer_base_address(SPI, 0, 0),

            ?TRACE("writing data to FIFO (len=~p): ~p~n", [byte_size(Data), Data]),
            _Response = sx126x_cmd:write_buffer(SPI, Data),

            ?TRACE("Populated buffer and setting TX mode to transmit...~n", []),
            ok = sx126x_cmd:set_tx(SPI, ?TX_HW_TIMEOUT),

            ?TRACE("Done broadcast~n", []),

            ok
    end.

%% @private
wait_channel_free(_SPI, _BusyPin, 0) ->
    {error, channel_busy};
wait_channel_free(SPI, BusyPin, Retries) ->
    case maybe_wait_until_not_busy(BusyPin, 100) of
        ok ->
            IRQFlags = sx126x_cmd:get_irq_status(SPI),
            case
                lists:member(preamble_detected, IRQFlags) orelse
                    lists:member(header_valid, IRQFlags)
            of
                true ->
                    timer:sleep(10),
                    wait_channel_free(SPI, BusyPin, Retries - 1);
                false ->
                    ok
            end;
        Error ->
            Error
    end.

%%%
%%% receive
%%%

%% @private
do_receive(State) ->
    SPI = State#state.spi,
    Config = State#state.config,

    ?TRACE("Receiving message", []),

    ok = sx126x_cmd:set_standby_xosc(SPI),

    IRQStatus = sx126x_cmd:get_irq_status(SPI),
    ?TRACE("IRQStatus: ~p", [IRQStatus]),

    Result =
        case lists:member(rx_done, IRQStatus) of
            false ->
                ?TRACE("No rx_done in IRQ status: ~p", [IRQStatus]),
                {error, no_rx_done};
            true ->
                case lists:member(crc_err, IRQStatus) of
                    true ->
                        ?TRACE("CRC error on receive!  Ignoring message.~n", []),
                        {error, crc_err};
                    false ->
                        case lists:member(header_err, IRQStatus) of
                            true ->
                                ?TRACE("Header error on receive!  Ignoring.~n", []),
                                {error, header_err};
                            false ->
                                {PayloadLengthRx, RxStartBufferPointer} = sx126x_cmd:get_rx_buffer_status(
                                    SPI
                                ),
                                ?TRACE("PayloadLengthRx: ~p RxStartBufferPointer: ~p", [
                                    PayloadLengthRx, RxStartBufferPointer
                                ]),

                                Payload = sx126x_cmd:read_buffer(
                                    SPI, RxStartBufferPointer, PayloadLengthRx
                                ),
                                ?TRACE("Payload: ~p~n", [Payload]),

                                QoS = get_qos(SPI),
                                {ok, Payload, QoS}
                        end
                end
        end,

    set_recv_mode(SPI, Config),

    case Result of
        {ok, Payload1, QoS1} ->
            notify_handler(Config, Payload1, QoS1);
        _Error ->
            ok
    end.

%% @private
notify_handler(Config, Payload, QoS) ->
    Lora = {?MODULE, self()},
    case maps:get(receive_handler, Config, undefined) of
        undefined ->
            ?TRACE("No receive handler configured for received message.  Ignoring message.", []),
            ok;
        Handler ->
            ReplyData =
                case maps:get(binary, Config, true) of
                    true -> Payload;
                    _ -> binary_to_list(Payload)
                end,
            if
                is_pid(Handler) ->
                    Handler ! {lora_receive, Lora, ReplyData, QoS};
                is_function(Handler) ->
                    spawn(fun() -> Handler(Lora, ReplyData, QoS) end);
                true ->
                    {error, unsupported_receive_handler}
            end
    end.

%% @private
get_qos(SPI) ->
    {Rssi, Snr, _SignalRssi} = sx126x_cmd:get_packet_status(SPI),
    #{
        rssi => Rssi,
        snr => Snr
    }.

%%%
%%% AGC reset
%%%

%% @private
do_agc_reset(State) ->
    SPI = State#state.spi,
    Config = State#state.config,
    BusyPin = State#state.busy_pin,

    ?TRACE("AGC reset", []),

    sx126x_cmd:set_sleep(SPI),
    timer:sleep(1),

    ok = sx126x_cmd:set_standby(SPI),
    _ = maybe_wait_until_not_busy(BusyPin, 100),

    ok = sx126x_cmd:calibration_all(SPI),
    _ = maybe_wait_until_not_busy(BusyPin, 100),

    Frequency = maps:get(frequency, Config, freq_915mhz),
    ok = sx126x_cmd:calibrate_image_for_frequency(SPI, Frequency),

    %% Re-apply settings cleared by calibration
    case maps:get(dio2_as_rf_switch, Config, true) of
        true -> ok = sx126x_cmd:set_dio2_as_rf_switch_ctl(SPI, enable);
        false -> ok
    end,

    ok = sx126x_cmd:set_rx_boosted_gain(SPI, maps:get(rx_boosted_gain, Config, true)),
    ok = sx126x_cmd:apply_rx_sensitivity_patch(SPI),

    set_recv_mode_full(SPI, Config),
    ok.

%%%
%%% helpers
%%%

%% @private
compute_ldro(Config) ->
    case maps:get(ldro, Config, auto) of
        auto ->
            SF = sf_numeric(maps:get(spreading_factor, Config, sf_7)),
            BwHz = maps:get(bandwidth_hz, Config, 125000),
            SymDurUs = (1 bsl SF) * 1000000 div BwHz,
            case SymDurUs > 16380 of
                true -> on;
                false -> off
            end;
        Explicit ->
            Explicit
    end.

%% @private
sf_numeric(sf_5) -> 5;
sf_numeric(sf_6) -> 6;
sf_numeric(sf_7) -> 7;
sf_numeric(sf_8) -> 8;
sf_numeric(sf_9) -> 9;
sf_numeric(sf_10) -> 10;
sf_numeric(sf_11) -> 11;
sf_numeric(sf_12) -> 12;
sf_numeric(N) when is_integer(N) -> N.

%% @private
do_sleep(SPI) ->
    ?TRACE("do_sleep", []),
    sx126x_cmd:set_sleep(SPI).
