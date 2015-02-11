%%%-------------------------------------------------------------------
%%% @author kuldeep <kuldeep@kuldeep-ubuntu>
%%% @copyright (C) 2015, kuldeep
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2015 by kuldeep <kuldeep@kuldeep-ubuntu>
%%%-------------------------------------------------------------------
-module(router_fsm).

-behaviour(gen_fsm).

%% API
-export([
	 start_link/0
	]).

%% gen_fsm callbacks
-export([
	 init/1, 

	 %% states
	 ready/2, ready/3, 
	 valid_fb/2, valid_fb/3,
	 %%invalid_fb/2, invalid_fb/3,
	 valid_rl/2, valid_rl/3,
	 %%invalid_rl/2, invalid_rl/3,
	 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

%% API : Event raiser
-export([raise_event/2]).

-define(SERVER, ?MODULE).
-define(MAX_LENGTH, 268435455).

-record(state, 
	{
	  apid, % PIDof associated Acceptor.
	  pkt_received, % Packet received from the acceptor
	  pkt_type, % Packet type determined by calling validate_fb()
	  rfsmpid % PID of self (router_FSM)  
	}
       ).

%%%===================================================================
%%% API
%%%===================================================================
raise_event(Pid, {validate_fb, Pkt}) ->
    gen_fsm:sync_send_event(Pid, {validate_fb, Pkt});
raise_event(Pid, {validate_rl, Pkt}) ->
    gen_fsm:sync_send_event(Pid, {validate_rl, Pkt});
raise_event(Pid, {route_pkt, Pkt}) ->
    gen_fsm:sync_send_event(Pid, {route_pkt, Pkt});
raise_event(Pid, stop) ->
    gen_fsm:send_all_state_event(Pid, stop).

    
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, state_name, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
ready(_Event, State) ->
    {next_state, ready, State}.
valid_fb(_Event, State) ->
    {next_state, valid_fb, State}.
%% invalid_fb(_Event, State) ->
%%     {next_state, invalid_fb, State}.
valid_rl(_Event, State) ->
    {next_state, valid_rl, State}.
%% invalid_rl(_Event, State) ->
%%     {next_state, invalid_rl, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
ready({validate_fb, Pkt}, _From, State) ->
    %% if valid change state else stop fsm
    Reply = validate_first_byte(PKT),
    {reply, Reply, valid_fb, State}.


valid_fb({validate_rl, Rest}, _From, State) ->
    %% if valid change state else stop fsm
    Reply = validate_remaining_length(Rest),
    {reply, Reply, valid_rl, State}.

valid_rl({route_pkt, Pkt}, _From, State) ->
    %% route packet and stop fsm
    Reply = route(Pkt_Type, Apid, Rest),
    {stop, normal, {ok, pkt_routed}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Validate first byte of the packet to determine its type and validate bit flags (bit#3 to bit#0)
validate_first_byte(<<1:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, connect, Rest};
validate_first_byte(<<2:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, connack, Rest};
validate_first_byte(<<3:4, _Dup:1, _QoS:2, _Retain:1, Rest/binary>>) -> 
   {ok, publish, Rest};
validate_first_byte(<<4:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, puback, Rest};
validate_first_byte(<<5:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, pubrec, Rest};
validate_first_byte(<<6:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
   {ok, pubrel, Rest};
validate_first_byte(<<7:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok,pubcomp, Rest};
validate_first_byte(<<8:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
    {ok, subscribe, Rest};
validate_first_byte(<<9:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, suback, Rest};
validate_first_byte(<<10:4, 0:1, 1:2, 0:1, Rest/binary>>) -> 
   {ok, unsubscribe, Rest};
validate_first_byte(<<11:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, unsuback, Rest};
validate_first_byte(<<12:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, pingreq, Rest};
validate_first_byte(<<13:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
   {ok, pingresp, Rest};
validate_first_byte(<<14:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, disconnect, Rest};
validate_first_byte(_) -> 
    {error, invalid_fb}.

%%===================================================================
%% Decode remaining length (RestBin does not contain FirstByte)
%% If value of the remaining length field is correct, return rest of the binary.
%% Rest of the binary returned will contain variable header and payload.
%% Rest of the binary returned will not contain fixed header.
%% If value of the remaining length field is not correct, return error.
validate_remaining_length(Rest) ->
    validate_remaining_length(Rest, 0, 1).

validate_remaining_length(_, RLength, _)
  when (RLength > ?MAX_LENGTH) ->
    {error, remaining_length_exceeds};
%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
validate_remaining_length(<<1:1, Len:7, Rest/binary>>, RLength, Multiplier) ->
    validate_remaining_length(Rest, RLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
validate_remaining_length(<<0:1, Len:7, Rest/binary>>, RLength, Multiplier)
    when ((RLength + Len * Multiplier) =:= size(Rest)) ->
    {ok, Rest};
%% Rest of the message is having invalid lenght.
validate_remaining_length(_, _, _) ->
    {error, invalid_rl}.

%%===================================================================
route(connect, Apid, Rest) ->
    connect:create(Apid, Rest),
    {ok, connect}.
