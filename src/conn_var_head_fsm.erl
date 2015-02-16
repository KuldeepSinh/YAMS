%% %% Copyright 2013, 2014, 2015 KuldeepSinh Chauhan
%% %%
%% %% Licensed under the Apache License, Version 2.0 (the "License");
%% %% you may not use this file except in compliance with the License.
%% %% You may obtain a copy of the License at
%% %%
%% %% http://www.apache.org/licenses/LICENSE-2.0
%% %%
%% %% Unless required by applicable law or agreed to in writing, software
%% %% distributed under the License is distributed on an "AS IS" BASIS,
%% %% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% %% See the License for the specific language governing permissions and
%% %% limitations under the License.

%% %%%-------------------------------------------------------------------
%% %%% @author  KuldeepSinh Chauhan
%% %%% @copyright (C) 2013, 2014, 2015 
%% %%% @doc
%% %%%
%% %%% @end
%% %%% Created : 15 Feb 2015 by  KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
-module(conn_var_head_fsm).

-behaviour(gen_fsm).

%% API
-export([
	 create/0, % Send request to the fsm to validate variable header of the packet.
	 start_link/0,
	 send_event/2 % Send event
	]).

%% gen_fsm callbacks
-export([
	 init/1, 
	 %% Possible States
	 ready/2, ready/3, 
	 valid_proto_name/2, valid_proto_name/3,
	 valid_proto_level/2, valid_proto_level/3,
	 valid_conn_flags/2, valid_conn_flags/3,
	 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

-define(SERVER, ?MODULE).

-record(flags, {user, password, will_retain, will_qos, will, clean_session, reserved}).
-record(var_head, {flags, kat}).
-record(state, {var_head, rest}).

%%%===================================================================
%%% API
%%%===================================================================
%% Send request to router_fsm_sup to create router
create() ->
    conn_var_head_fsm_sup:start_child().

send_event(VarHeadFSMPid, {validate_proto_name, Pkt}) ->
    gen_fsm:sync_send_event(VarHeadFSMPid, {validate_proto_name, Pkt});
send_event(VarHeadFSMPid, {validate_proto_level}) ->
    gen_fsm:sync_send_event(VarHeadFSMPid, {validate_proto_level});
send_event(VarHeadFSMPid, {validate_conn_flags}) ->
    gen_fsm:sync_send_event(VarHeadFSMPid, {validate_conn_flags});
send_event(VarHeadFSMPid, {extract_kat}) ->
    gen_fsm:sync_send_event(VarHeadFSMPid, {extract_kat}).


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
    {ok, ready, #state{}}.

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
valid_proto_name(_Event, State) ->
    {next_state, valid_proto_name, State}. 
valid_proto_level(_Event, State) ->
    {next_state, valid_proto_level, State}. 
valid_conn_flags(valid_conn_flags, State) ->
    {next_state, valid_conn_flags, State}. 

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
ready({validate_proto_name, <<0:8, 4:8, "MQTT", Rest/binary>>}, _From, State) ->
    NewState = State#state{rest=Rest},
    {reply, valid, valid_proto_name, NewState};
ready(_, _, State) ->
    {stop, normal, {error, invalid_proto_name}, State}.

valid_proto_name({validate_proto_level}, _From, #state{var_head = _, rest = <<4:8, Rest/binary>>} = State) ->
    NewState = State#state{rest=Rest},
    {reply, valid, valid_proto_level, NewState};
valid_proto_name(_, _, State) ->
    {stop, normal, {error, invalid_proto_level}, State}.

valid_proto_level({validate_conn_flags}, _From, State) ->
    {reply, valid, valid_conn_flags, State}.
valid_conn_flags({extract_kat}, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State}.

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
handle_sync_event(stop, _From, StateName, State) ->
    Reply = ok,
    {stop, normal, StateName, State}.

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

%% %% Copyright 2013 KuldeepSinh Chauhan
%% %%
%% %% Licensed under the Apache License, Version 2.0 (the "License");
%% %% you may not use this file except in compliance with the License.
%% %% You may obtain a copy of the License at
%% %%
%% %% http://www.apache.org/licenses/LICENSE-2.0
%% %%
%% %% Unless required by applicable law or agreed to in writing, software
%% %% distributed under the License is distributed on an "AS IS" BASIS,
%% %% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% %% See the License for the specific language governing permissions and
%% %% limitations under the License.

%% %%%-------------------------------------------------------------------
%% %%% @author  KuldeepSinh Chauhan
%% %%% @copyright (C) 2013, 
%% %%% @doc
%% %%%
%% %%% @end
%% %%% Created : 17 Aug 2013 by  KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
%% -module(connect).

%% -behaviour(gen_server).

%% %% API
%% -export([start_link/2,
%% 	 create/2,
%% 	 stop/1]).

%% %% gen_server callbacks
%% -export([init/1, 
%% 	 handle_call/3, 
%% 	 handle_cast/2, 
%% 	 handle_info/2,
%% 	 terminate/2, 
%% 	 code_change/3]).

%% -define(SERVER, ?MODULE). 

%% -record(state, {apid, msg, self, flags, kat, payload, wills, user, pswd}).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server
%% %%
%% %% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link(APid, Msg) ->
%%     gen_server:start_link(?MODULE, [APid, Msg], []).

%% create(APid, Msg) ->
%%     connect_sup:start_child(APid, Msg).

%% stop(CPid) ->
%%     gen_server:call(CPid, stop).
%% %%%===================================================================
%% %%% gen_server callbacks
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Initializes the server
%% %%
%% %% @spec init(Args) -> {ok, State} |
%% %%                     {ok, State, Timeout} |
%% %%                     ignore |
%% %%                     {stop, Reason}
%% %% @end
%% %%--------------------------------------------------------------------
%% init([APid, Msg]) ->
%%     {ok, #state{apid = APid, msg = Msg, self = self()}, 0}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling call messages
%% %%
%% %% @spec handle_call(Request, From, State) ->
%% %%                                   {reply, Reply, State} |
%% %%                                   {reply, Reply, State, Timeout} |
%% %%                                   {noreply, State} |
%% %%                                   {noreply, State, Timeout} |
%% %%                                   {stop, Reason, Reply, State} |
%% %%                                   {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling cast messages
%% %%
%% %% @spec handle_cast(Msg, State) -> {noreply, State} |
%% %%                                  {noreply, State, Timeout} |
%% %%                                  {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_cast(stop, State ) ->
%%     {stop, normal, State};
%% handle_cast(_Message, State) ->
%%    {noreply, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling all non call/cast messages
%% %%
%% %% @spec handle_info(Info, State) -> {noreply, State} |
%% %%                                   {noreply, State, Timeout} |
%% %%                                   {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_info(timeout, #state{self = CPid} = State) ->
%%     %% <ToDo> Implement FSM to process message.
%%     process_message(State),   
%%     stop(CPid),
%%     {noreply, State};
%% handle_info(_Info, State) ->
%%     {noreply, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% This function is called by a gen_server when it is about to
%% %% terminate. It should be the opposite of Module:init/1 and do any
%% %% necessary cleaning up. When it returns, the gen_server terminates
%% %% with Reason. The return value is ignored.
%% %%
%% %% @spec terminate(Reason, State) -> void()
%% %% @end
%% %%--------------------------------------------------------------------
%% terminate(_Reason, _State) ->
%%     ok.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Convert process state when code is changed
%% %%
%% %% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% %% @end
%% %%--------------------------------------------------------------------
%% code_change(_OldVsn, State, _Extra) ->
%%     {ok, State}.

%% %%%===================================================================
%% %%% Internal functions
%% %%%===================================================================
%% %% CONNACK = Acknowledge CONNECT
%% connack(0) -> {ok, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, 0:8>>};
%% connack(Code) -> {error, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, Code:8>>}.

%% %% ================================
%% %% Start processing the message.
%% process_message(State) ->    
%%     validate_protocol(State).

%% %% ================================
%% %% Validate protocol name and version.
%% validate_protocol(#state{msg = <<6:16, "MQIsdp", 3:8, _Rest/binary>>} = State) ->
%%     get_flags(State);

%% %% if protocol name or version is invalid, send connack with code = 1.
%% validate_protocol(#state{apid = APid}) ->
%%      acceptor:connack(APid, connack(1)).

%% %% ================================
%% %% Get connection flags.
%% get_flags(#state{msg = <<_:72, Usr:1, Pwd:1, WillR:1, WillQ:2, Will:1, ClnS:1, Rsvd:1, _Rest/binary>>} = State) ->
%%     NewState = State#state{flags = {con_flags, Usr, Pwd, WillR, WillQ, Will, ClnS, Rsvd}},
%%     get_kat_payload(NewState).

%% %% ================================
%% %% Split KAT and the Payload apart. 
%% get_kat_payload(#state{msg = <<_:80, KAT:16, Payload/binary>>} = State) ->
%%     NewState = State#state{kat = KAT, payload = Payload},
%%     split_payload(NewState).

%% %% ================================
%% %% Split payload into the given fields.
%% split_payload(#state{payload = Payload} = State) ->
%%     PL = splt_pld(Payload), %splt_pld is defined below.
%%     NewState = State#state{payload = lists:reverse(PL)},
%%     validate_client(NewState).

%% %% Split payload into the list of {FieldLength, Field}
%% %% Note: caller should reverse the list returned from this function.
%% splt_pld(<<>>) ->
%%     [];
%% splt_pld(<<L:16, Rest/binary>>)
%%     when (size(Rest) >= L) ->
%%     {Extract, RestBin} = split_binary(Rest, L),
%%     splt_pld(RestBin) ++ [{L, Extract}];
%% splt_pld(_) ->
%%     {error, length_mismatch}.

%% %% ================================
%% %% Validate client ID (it is a dummy implementation right now).
%% validate_client(#state{apid = APid, payload = [{L, ID} | _]} = State) ->
%%     case vldt_client({L, ID}) of %A dummy vldt_client is defined below.
%% 	{ok, _, _} ->
%% 	    get_wills(State);
%% 	{error, _} ->	    
%% 	    acceptor:connack(APid, connack(2))
%%     end.

%% %% Validate client identifier
%% vldt_client({L, Val})
%%   when((L >= 1) and (L =< 23)) ->
%%     %%<ToDo> : Lookup in the client-registry if the client ID is unique/client is registered with the system.
%%     {ok, valid_client, binary_to_list(Val)};
%% vldt_client(_) ->
%%     {error, invalid_client}.

%% %% ================================
%% %% When Will flag is equal to 1, Collect will topic and will message into the state record.
%% get_wills(#state{flags = {con_flags, _Usr, _Pwd, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) ->
%%     Topic = lists:nth(2, Payload),
%%     Msg = lists:nth(3, Payload),
%%     NewState = State#state{wills = {Topic, Msg}}, 
%%     get_user(NewState);

%% %% When Will flag is NOT equal to 1, call get user.
%% get_wills(State) ->
%%     get_user(State).

%% %% ================================
%% %% When User flag = 1 and Will flag = 1, get the user.
%% get_user(#state{flags  = {con_flags, 1, _Pwd, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) 
%%   when(erlang:length(Payload) >= 4) ->
%%     User = lists:nth(4, Payload),
%%     NewState = State#state{user = User},
%%     get_pswd(NewState);

%% %% When User flag = 1 and Will flag = 0, get the user.
%% get_user(#state{flags  = {con_flags, 1, _Pwd, _WillR, _WillQ, 0, _ClnS, _Rsvd}, payload = Payload} = State) 
%%   when(erlang:length(Payload) >= 2) ->
%%     User = lists:nth(2, Payload),
%%     NewState = State#state{user = User},
%%     get_pswd(NewState);

%% %% When User flag = 0, call authentication -
%% %% Rational to call authentication - We may allow unregistered users as the guest users.
%% get_user(State) ->
%%     authenticate(State).

%% %% ================================
%% %% When User flag = 1, Password flag = 1  and Will flag = 1, get the password.
%% get_pswd(#state{flags  = {con_flags, 1, 1, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) 
%%   when(erlang:length(Payload) == 5) ->
%%     Pswd = lists:nth(5, Payload),
%%     NewState = State#state{pswd = Pswd},
%%     authenticate(NewState);

%% %% When User flag = 1, Password flag = 1  and Will flag = 0, get the password.
%% get_pswd(#state{flags  = {con_flags, 1, 1, _WillR, _WillQ, 0, _ClnS, _Rsvd}, payload = Payload} = State) 
%%   when(erlang:length(Payload) == 3) ->
%%     Pswd = lists:nth(3, Payload),
%%     NewState = State#state{pswd = Pswd},
%%     authenticate(NewState);

%% %% When User flag = 1, Password flag = 0, authenticate user.
%% get_pswd(#state{flags  = {con_flags, 1, 0, _WillR, _WillQ, _Will, _ClnS, _Rsvd}} = State) ->
%%     authenticate(State).

%% %% ================================
%% %% dummy implementation for user authentication.
%% authenticate(#state{apid = APid, user = _User, pswd = _Psw, payload = [{_L, ClientID} | _], kat = KAT}) ->
%%     authorize(APid, ClientID, KAT).

%% %% ================================
%% %% dummy implementation for user authorization.
%% authorize(APid, ClientID, KAT) ->
%%     %% Send client ID, so that acceptor will be able to 
%%     %% enter an entry in the directory for client-apid mapping.
%%     acceptor:connack(APid, {ClientID, KAT, connack(0)}).
