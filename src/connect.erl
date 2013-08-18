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
-module(connect).

-behaviour(gen_server).

%% API
-export([start_link/2,
	 create/2]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {apid, msg, self, flags, kat, payload, wills, user, pswd}).

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
start_link(APid, Msg) ->
    gen_server:start_link(?MODULE, [APid, Msg], []).

create(APid, Msg) ->
    connect_sup:start_child(APid, Msg).

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
init([APid, Msg]) ->
    {ok, #state{apid = APid, msg = Msg, self = self()}, 0}.

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
handle_cast(_Message, State) ->
   {noreply, State}.



validate_protocol(#state{msg = <<6:16, "MQIsdp", 3:8, _Rest/binary>>} = State) ->
    get_flags(State);
validate_protocol(#state{apid = APid}) ->
	{ok, Ack} = connack(1),
    acceptor:reply(APid, Ack).
get_flags(#state{msg = <<_:72, Usr:1, Pwd:1, WillR:1, WillQ:2, Will:1, ClnS:1, Rsvd:1, _Rest/binary>>} = State) ->
    NewState = State#state{flags = {con_flags, Usr, Pwd, WillR, WillQ, Will, ClnS, Rsvd}},
    get_kat_payload(NewState).
get_kat_payload(#state{msg = <<_:80, KAT:16, Payload/binary>>} = State) ->
    NewState = State#state{kat = KAT, payload = Payload},
    split_payload(NewState).
split_payload(#state{payload = Payload} = State) ->
    PL = split_pload(Payload),
    NewState = State#state{payload = lists:reverse(PL)},
    validate_client(NewState).
validate_client(#state{apid = APid, payload = [{L, ID} | _]} = State) ->
    case vldt_client({L, ID}) of
	{ok, _, _} ->
	    get_wills(State);
	{error, _} ->
		{ok, Ack} = connack(2),
	    acceptor:reply(APid, Ack)
    end.
get_wills(#state{flags = {con_flags, _Usr, _Pwd, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) ->
    Topic = lists:nth(2, Payload),
    Msg = lists:nth(3, Payload),
    NewState = State#state{wills = {Topic, Msg}}, 
    get_user(NewState).
get_user(#state{flags  = {con_flags, 1, _Pwd, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) 
  when(erlang:length(Payload) >= 4) ->
    User = lists:nth(4, Payload),
    NewState = State#state{user = User},
    get_pswd(NewState);
get_user(#state{flags  = {con_flags, 1, _Pwd, _WillR, _WillQ, 0, _ClnS, _Rsvd}, payload = Payload} = State) 
  when(erlang:length(Payload) >= 2) ->
    User = lists:nth(2, Payload),
    NewState = State#state{user = User},
    get_pswd(NewState).
get_pswd(#state{flags  = {con_flags, 1, 1, _WillR, _WillQ, 1, _ClnS, _Rsvd}, payload = Payload} = State) 
  when(erlang:length(Payload) == 5) ->
    Pswd = lists:nth(5, Payload),
    NewState = State#state{pswd = Pswd},
    authenticate(NewState);
get_pswd(#state{flags  = {con_flags, 1, 1, _WillR, _WillQ, 0, _ClnS, _Rsvd}, payload = Payload} = State) 
  when(erlang:length(Payload) == 3) ->
    Pswd = lists:nth(3, Payload),
    NewState = State#state{pswd = Pswd},
    authenticate(NewState).
authenticate(#state{apid = APid, user = _User, pswd = _Psw}) ->
    authorize(APid).
authorize(APid) ->
	{ok, Ack} = connack(0),
    acceptor:reply(APid, Ack).

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
handle_info(timeout, State) ->
    %gen_server:handle_cast(Self, validate_protocol),
    validate_protocol(State),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% CONNACK = Acknowledge CONNECT
connack(0) -> {ok, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, 0:8>>};
connack(Code) -> {error, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, Code:8>>}.

%% Split payload into the list of {FieldLength, Field}
%% Note: caller should reverse the list returned from this function.
split_pload(<<>>) ->
    [];
split_pload(<<L:16, Rest/binary>>)
    when (size(Rest) >= L) ->
    {Extract, RestBin} = split_binary(Rest, L),
    split_pload(RestBin) ++ [{L, Extract}];
split_pload(_) ->
    {error, length_mismatch}.

%% Validate client identifier
vldt_client({L, Val})
  when((L >= 1) and (L =< 23)) ->
    %%<ToDo> : Lookup in the client-registry if the client ID is unique/client is registered with the system.
    {ok, valid_client, binary_to_list(Val)};
vldt_client(_) ->
    {error, invalid_client}.
