%% Copyright 2013, 2014, 2015 KuldeepSinh Chauhan
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%%-------------------------------------------------------------------
%%% @author  KuldeepSinh Chauhan
%%% @copyright (C) 2013, 2014, 2015
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2015 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(conn_payload_svr).

-behaviour(gen_server).

%% API
-export([
	 start_link/1,
	 create/1,
	 stop/1,
	 group_flags_and_fields/1
	]).

%% gen_server callbacks
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3
	]).

-define(SERVER, ?MODULE).

%% Included "connect.hrl" contains definitions of below mentioned records.
%% -record(conn_flags, {user, password, will_retain, will_qos, will, clean_session}).
%% -record(conn_var_head, {conn_flags, kat}).
%% -record(conn_pkt, {conn_var_head, payload}). 
%% -record(client, {client_id, username, password}). 
%% -record(conn_will,{client, will_flag, will_qos, will_retain, will_topic, will_msg}).
-include("../include/connect.hrl").
%%%===================================================================
%%% API
%%%===================================================================
create(Pkt) ->
    conn_payload_svr_sup:start_child(Pkt).

stop(SelfPid) ->
    gen_server:cast(SelfPid, stop).

group_flags_and_fields(SelfPid) ->
    gen_server:call(SelfPid, group_flags_and_fields).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Pkt) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Pkt], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Pkt]) ->
    {ok, Pkt}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(group_flags_and_fields, _From, Pkt) ->
    Reply = extract_password
	       (extract_user
		  (extract_will_msg
		     (extract_will_topic
			(extract_client_id(Pkt))))),
    {reply, Reply, Pkt}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State ) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
extract_client_id(#conn_pkt{conn_var_head = ConnVarHead, payload = Payload}) ->
    get_client(yams_lib:extract_str(Payload), ConnVarHead).

get_client({error, Reason}, _ConnVarHead) ->
    {error, Reason};
get_client({ok, _Length, Value, RestBin}, ConnVarHead) ->
    {ok, RestBin, ConnVarHead, #client{client_id = Value}}.

extract_will_topic({error, Reason}) ->
    {error, Reason};
extract_will_topic({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{will = 1}} = ConnVarHead, Client}) ->
    get_will_topic(yams_lib:extract_str(RestBin), ConnVarHead, Client);
extract_will_topic({ok, RestBin, ConnVarHead, Client, #conn_will{}}) ->
    {ok, RestBin, ConnVarHead, Client}.

get_will_topic({error, Reason}, _ConnVarHead, _Client) ->
    {error, Reason};
get_will_topic({ok, _Length, Value, RestBin}, #conn_var_head{conn_flags = #conn_flags{will = Will, will_qos = WillQoS, will_retain = WillRetain}} = ConnVarHead, Client) ->
    {ok, RestBin, ConnVarHead, Client, #conn_will{client = Client, will = Will, will_qos = WillQoS, will_retain = WillRetain, will_topic = Value}}.

extract_will_msg({error, Reason}) ->
    {error, Reason};
extract_will_msg({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{will = 1}} = ConnVarHead, Client, Will}) ->
    get_will_msg(yams_lib:extract_str(RestBin), ConnVarHead, Client, Will);
extract_will_msg({ok, RestBin, ConnVarHead, Client, Will}) ->
    {ok, RestBin, ConnVarHead, Client, Will}.

get_will_msg({error, Reason}, _ConnVarHead, _Client, _Will) ->
    {error, Reason};
get_will_msg({ok, _Length, Value, RestBin}, ConnVarHead, Client, Will) ->
    {ok, RestBin, ConnVarHead, Client, Will#conn_will{will_msg = Value}}.


extract_user({error, Reason}) ->
    {error, Reason};
extract_user({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{user = 1}} = ConnVarHead, Client, Will}) ->
    get_user(yams_lib:extract_str(RestBin), ConnVarHead, Client, Will);
extract_user({ok, RestBin, ConnVarHead, Client, Will}) ->
    {ok, RestBin, ConnVarHead, Client, Will}.

get_user({error, Reason}, _ConnVarHead, _Client, _Will) ->
    {error, Reason};
get_user({ok, _Length, Value, RestBin}, ConnVarHead, Client, Will) ->
    {ok, RestBin, ConnVarHead, Client#client{username = Value}, Will#conn_will{client = Client#client{username = Value}}}.

extract_password({error, Reason}) ->
    {error, Reason};
extract_password({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{password = 1}} = ConnVarHead, Client, Will}) ->
    get_password(yams_lib:extract_str(RestBin), ConnVarHead, Client, Will);
extract_password({ok, RestBin, ConnVarHead, Client, Will}) ->
    {ok, RestBin, ConnVarHead, Client, Will}.

get_password({error, Reason}, _ConnVarHead, _Client, _Will) ->
    {error, Reason};
get_password({ok, _Length, Value, RestBin}, ConnVarHead, Client, Will) ->
    {ok, RestBin, ConnVarHead, Client#client{password = Value}, Will#conn_will{client = Client#client{password = Value}}}.


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
