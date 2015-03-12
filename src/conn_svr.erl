%% Copyright 2013 KuldeepSinh Chauhan
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
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Aug 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(conn_svr).

-behaviour(gen_server).

%% API
-export([
	 create/2,
	 start_link/2,
	 stop/1
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
-include("../include/connect.hrl").
-record(state, 
	{
	  apid, 
	  self, 
	  pkt
	}
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(APid, Pkt) ->
    conn_svr_sup:start_child(APid, Pkt).

start_link(APid, Pkt) ->
    gen_server:start_link(?MODULE, [APid, Pkt], []).

stop(CPid) ->
    gen_server:cast(CPid, stop).

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
init([APid, Pkt]) ->
    {ok, #state{apid = APid, self = self(), pkt = Pkt}, 0}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    {stop, normal, State};
handle_cast(_Message, State) ->
   {noreply, State}.

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
handle_info(timeout, #state{apid = APid, self = CPid, pkt = Pkt} = State) ->
    process_conn_pkt(process_var_head(Pkt)),    
    stop(CPid),
    {noreply, State};
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

%% ===================================================================
%% Internal functions
%% ===================================================================

process_var_head(Pkt) ->
    {ok, VarHeadSvrPid, ReplyReceived} = separate_varhead_n_payload(create_var_head_svr(Pkt)),
    stop_var_head_svr(VarHeadSvrPid),
    ReplyReceived.    
%% create conn_var_head_svr
create_var_head_svr(Pkt) ->
    Result = conn_var_head_svr:create(Pkt), %% where, Result = {ok, VarHeadSvrPid}
    Result.
%% separate varhead and payload from the Packet.
separate_varhead_n_payload({ok, VarHeadSvrPid}) ->
    ConnPkt = conn_var_head_svr:validate_var_head(VarHeadSvrPid), %% where, ConnPkt = #conn_pkt{}
    {ok, VarHeadSvrPid, ConnPkt}.  
stop_var_head_svr(VarHeadSvrPid) ->
    conn_var_head_svr:stop(VarHeadSvrPid).

process_conn_pkt({error, Reason}) ->
    {error, Reason};
process_conn_pkt({ok, ConnPkt}) ->
    {ok, PayloadSvrPid, ReplyReceived} = group_flags_n_fields(create_payload_svr(ConnPkt)),
    stop_payload_svr(PayloadSvrPid),
    ReplyReceived.    
%% create conn_paylaod_svr
create_payload_svr(ConnPkt) ->
    Result = conn_payload_svr:create(ConnPkt), %% where, Result = {ok, ConnPayloadSvrPid}
    Result.
%% separate varhead and payload from the Packet.
group_flags_n_fields({ok, PayloadSvrPid}) ->
    Group = conn_payload_svr:group_flags_and_fields(PayloadSvrPid), 
    {ok, PayloadSvrPid, Group}.  
stop_payload_svr(PayloadSvrPid) ->
    conn_payload_svr:stop(PayloadSvrPid).
