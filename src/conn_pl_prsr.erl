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
-module(conn_pl_prsr).

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
    conn_pl_prsr_sup:start_child(Pkt).

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
    {reply, prepare_reply(Reply), Pkt}.

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
extract_will_topic({ok, RestBin, ConnVarHead, Client}) ->
    {ok, RestBin, ConnVarHead, Client, #conn_will{}}.

get_will_topic({error, Reason}, _ConnVarHead, _Client) ->
    {error, Reason};
get_will_topic({ok, _Length, Value, RestBin}, #conn_var_head{conn_flags = #conn_flags{will = Will, will_qos = WillQoS, will_retain = WillRetain}} = ConnVarHead, Client) ->
    {ok, RestBin, ConnVarHead, Client, #conn_will{client = Client, will = Will, will_qos = WillQoS, will_retain = WillRetain, will_topic = Value}}.

extract_will_msg({error, Reason}) ->
    {error, Reason};
extract_will_msg({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{will = 1}} = ConnVarHead, Client, ConnWill}) ->
    get_will_msg(yams_lib:extract_str(RestBin), ConnVarHead, Client, ConnWill);
extract_will_msg({ok, RestBin, ConnVarHead, Client, ConnWill}) ->
    {ok, RestBin, ConnVarHead, Client, ConnWill}.

get_will_msg({error, Reason}, _ConnVarHead, _Client, _ConnWill) ->
    {error, Reason};
get_will_msg({ok, _Length, Value, RestBin}, ConnVarHead, Client, ConnWill) ->
    {ok, RestBin, ConnVarHead, Client, ConnWill#conn_will{will_msg = Value}}.


extract_user({error, Reason}) ->
    {error, Reason};
extract_user({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{user = 1}} = ConnVarHead, Client, ConnWill}) ->
    get_user(yams_lib:extract_str(RestBin), ConnVarHead, Client, ConnWill);
extract_user({ok, RestBin, ConnVarHead, Client, ConnWill}) ->
    {ok, RestBin, ConnVarHead, Client, ConnWill}.

get_user({error, Reason}, _ConnVarHead, _Client, _ConnWill) ->
    {error, Reason};
get_user({ok, _Length, Value, RestBin}, ConnVarHead, Client, ConnWill) ->
    {ok, RestBin, ConnVarHead, Client#client{username = Value}, ConnWill#conn_will{client = Client#client{username = Value}}}.

extract_password({error, Reason}) ->
    {error, Reason};
extract_password({ok, RestBin, #conn_var_head{conn_flags = #conn_flags{password = 1}} = ConnVarHead, Client, ConnWill}) ->
    get_password(yams_lib:extract_str(RestBin), ConnVarHead, Client, ConnWill);
extract_password({ok, RestBin, ConnVarHead, Client, ConnWill}) ->
    {ok, RestBin, ConnVarHead, Client, ConnWill}.

get_password({error, Reason}, _ConnVarHead, _Client, _ConnWill) ->
    {error, Reason};
get_password({ok, _Length, Value, RestBin}, ConnVarHead, Client, ConnWill) ->
    {ok, RestBin, ConnVarHead, Client#client{password = Value}, ConnWill#conn_will{client = Client#client{password = Value}}}.

prepare_reply({ok, <<>>, _ConnVarHead, Client, ConnWill}) ->
    {ok, Client, ConnWill};
prepare_reply(Error) ->
    Error.
