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
%%% Created : 24 Feb 2015 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(conn_var_head_svr).

-behaviour(gen_server).

%% API
-export([
	 start_link/1,
	 create/1,
	 stop/1,
	 validate_var_head/1
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

%% Included "connect.hrl" contains definitions of conn_flags and conn_var_head.
%%-record(conn_flags, {user, password, will_retain, will_qos, will, clean_session}).
%%-record(conn_var_head, {conn_flags, kat}).
%%-record(conn_pkt, {conn_var_head, payload}). 
%%       conn_pkt will store state of the fsm. Same state will be returned to conn_svr for further processing.
-include("../include/connect.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create(Pkt) ->
    conn_var_head_svr_sup:start_child(Pkt).

stop(SelfPid) ->
    gen_server:cast(SelfPid, stop).

validate_var_head(SelfPid) ->
    gen_server:call(SelfPid, validate_var_head).

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
    {ok, #conn_pkt{payload = Pkt}}.

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
handle_call(validate_var_head, _From, #conn_pkt{payload = Pkt} = State) ->
    ProcessedPkt = extract_kat
		     (validate_password_flag
			(validate_will_flags
			   (validate_reserved_flag
			      (extract_flags
				 (validate_proto_level
				    (validate_proto_name(Pkt))))))),
    {reply, prepare_reply(ProcessedPkt), State}. 

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

%% Following function validates Protocol name.
validate_proto_name (<<0:8, 4:8, "MQTT", Rest/binary>>) ->
    {ok, valid_proto_name, Rest}; %% return value = {ok, Reason, ReturnValue}
validate_proto_name (_Pkt) ->
    {error, invalid_proto_name}. %% return value = {error, Reason}

%% Following function validates Protocol level.
validate_proto_level({error, Reason}) -> 
    {error, Reason};
validate_proto_level({ok, valid_proto_name, <<4:8, Rest/binary>>}) ->
    {ok, valid_proto_level, Rest};
validate_proto_level(_Pkt) ->
    {error, unacceptable_proto_level}. %% return value = {error, Reason}

%% Following function extracts Connect Flags from the variable header/rest of the binary.
extract_flags({error, Reason}) ->
    {error, Reason};
%% Extract each flag from the first 7 bits of the packet.
extract_flags({ok, valid_proto_level, <<User:1, Password:1, Will_retain:1, Will_qos:2, Will:1, Clean_session:1, Reserved:1, Rest/binary>>}) ->
    ConnFlags = #conn_flags
	{
	  user = User, 
	  password=Password, 
	  will_retain=Will_retain, 
	  will_qos=Will_qos, 
	  will=Will, 
	  clean_session=Clean_session, 
	  reserved = Reserved
	},    
    VarHead = #conn_var_head{conn_flags = ConnFlags},
    ConnPkt = #conn_pkt{conn_var_head = VarHead, payload=Rest},
    {ok, conn_flags_extracted, ConnPkt}.

%% validate reserved flag.
%% The Server MUST validate that the reserved flag in the CONNECT Control Packet is set to zero and disconnect the Client if it is not zero [MQTT-3.1.2-3].
validate_reserved_flag({error, Reason}) ->
    {error, Reason};
validate_reserved_flag({ok, conn_flags_extracted, ConnPkt}) -> 
    validate_reserved_flag(ConnPkt);
%% When reserved flag is set to 0, its valid.
validate_reserved_flag(#conn_pkt{conn_var_head = #conn_var_head{conn_flags = #conn_flags{reserved = 0}}} = ConnPkt) ->
    {ok, valid_reserved_flag, ConnPkt};
validate_reserved_flag(_) ->
    {error, invalid_reserved_flag}.


%% Validate will_retain and will_qos flags - based on the value of the will flag.
%% If the Will Flag is set to 0 the Will QoS and Will Retain fields in the Connect Flags MUST be set to zero and the Will Topic and Will Message fields MUST NOT be present in the payload [MQTT-3.1.2-11]
validate_will_flags({error, Reason}) ->
    {error, Reason};
%% If the Will Flag is set to 0, then the Will QoS MUST be set to 0 (0x00) [MQTT-3.1.2-13]
%% If the Will Flag is set to 0, then the Will Retain Flag MUST be set to 0 [MQTT-3.1.2-15].
validate_will_flags({ok, valid_reserved_flag, ConnPkt}) ->
    validate_will_flags(ConnPkt);
validate_will_flags(#conn_pkt{conn_var_head = #conn_var_head{conn_flags = #conn_flags{will_retain = 0, will_qos = 0, will = 0}}} = ConnPkt) ->
    {ok, valid_will_flags, ConnPkt};
%% If the Will Flag is set to 0 the Will QoS and Will Retain fields in the Connect Flags MUST be set to zero and the Will Topic and Will Message fields MUST NOT be present in the payload [MQTT-3.1.2-11]
%% If the Will Flag is set to 0, then the Will QoS MUST be set to 0 (0x00) [MQTT-3.1.2-13]
%% If the Will Flag is set to 0, then the Will Retain Flag MUST be set to 0 [MQTT-3.1.2-15].
validate_will_flags(#conn_pkt{conn_var_head = #conn_var_head{conn_flags = #conn_flags{will = 0}}}) ->
    {error, invalid_will_flags};
%% If the Will Flag is set to 1, the value of Will QoS can be 0 (0x00), 1 (0x01), or 2 (0x02). It MUST NOT be 3 (0x03) [MQTT-3.1.2-14].
%% Combining this rule with above pattern - will_qos will never be equal to 3 for any value of will flag.
validate_will_flags(#conn_pkt{conn_var_head = #conn_var_head{conn_flags = #conn_flags{will_qos = 3}}}) ->
    {error, invalid_will_qos};
%% If the Will Flag is set to 1, the value of Will QoS can be 0 (0x00), 1 (0x01), or 2 (0x02). It MUST NOT be 3 (0x03) [MQTT-3.1.2-14].
%% Combining this rule with above patterns - rest of the will flags will be valid.
validate_will_flags(ConnPkt) ->
    {ok, valid_will_flags, ConnPkt}.

%% Validate password flag - based on the value of the username flag.
validate_password_flag({error, Reason}) ->
    {error, Reason};
validate_password_flag({ok, valid_will_flags, ConnPkt}) ->
    validate_password_flag(ConnPkt);
validate_password_flag(#conn_pkt{conn_var_head = #conn_var_head{conn_flags = #conn_flags{user = 0, password = 1}}}) ->
    {error, invalid_password_flag};
%% rest of the patterns are valid.
%% (If the User Name Flag is set to 0, the Password Flag MUST be set to 0 [MQTT-3.1.2-22].)
validate_password_flag(ConnPkt) ->
    {ok, valid_password_flag, ConnPkt}.

%% Extract KAT from the payload.
extract_kat({error, Reason}) ->
    {error, Reason};
extract_kat({ok, valid_password_flag, #conn_pkt{conn_var_head = VarHead, payload = <<KAT:16, Rest/binary>>} = ConnPkt}) ->
    NewVarHead = VarHead#conn_var_head{kat = KAT},
    NewConnPkt = ConnPkt#conn_pkt{conn_var_head = NewVarHead, payload = Rest},
    {ok, valid_kat_value, NewConnPkt};
extract_kat(_) ->
    {error, invalid_kat_value}.

prepare_reply({ok, valid_kat_value, NewConnPkt}) ->
    {ok, NewConnPkt};
prepare_reply(Error) ->
    Error.    
