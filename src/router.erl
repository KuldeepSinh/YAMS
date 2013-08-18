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
%%%     This module will route messages to message handlers in accordance with their type.
%%% @end
%%% Created : 15 Aug 2013 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(router).

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
-define(MAX_LENGTH, 268435455).

-record(state, {apid, msg}).

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
    router_sup:start_child(APid, Msg).
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
    {ok, #state{apid = APid, msg = Msg}, 0}.

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
handle_cast(_Msg, State) ->
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
handle_info(timeout, #state{apid = APid, msg = Msg} = State) ->
    {ok, _Type} = route(APid, Msg),
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
%% Identify message type.
route(APid, <<1:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    connect:create(APid, RestMsg),
    {ok, connect};
route(APid, <<2:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, connack};
route(APid, <<3:4, Dup:1, QoS:2, Retain:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, publish};
route(APid, <<4:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest),     
    {ok, puback};
route(APid, <<5:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest),  
    {ok, pubrec};
route(APid, <<6:4, Dup:1, QoS:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, pubrel};
route(APid, <<7:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, pubcomp};
route(APid, <<8:4, Dup:1, QoS:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, subscribe};
route(APid, <<9:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, suback};
route(APid, <<10:4, Dup:1, QoS:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, unsubscribe};
route(APid, <<11:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, unsuback};
route(APid, <<12:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, pingreq};
route(APid, <<13:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, pingresp};
route(APid, <<14:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
    {ok, RestMsg} = get_rest_bin(Rest), 
    {ok, disconnect}.

%% Decode remaining length (RestBin does not contain FirstByte)
get_rest_bin(Rest) ->
    get_rest_bin(Rest, 0, 1).
get_rest_bin(_, RLength, _)
  when (RLength > ?MAX_LENGTH) ->
    {error, remaining_length_exceeds};
%% Calculate the remaining length value:
%% Recurse if the value of the first bit is 1.
get_rest_bin(<<1:1, Len:7, Rest/binary>>, RLength, Multiplier) ->
    get_rest_bin(Rest, RLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length :
%% Return if the value of the first bit is 0.
get_rest_bin(<<0:1, Len:7, Rest/binary>>, RLength, Multiplier)
    when ((RLength + Len * Multiplier) =:= size(Rest)) ->
    {
      ok, Rest
      %% <ToDo : Remaining length helpful for debugging. I should log it's value (may be with 'lager').>
      %% {remaining_length, RLength + Len * Multiplier},
      %% {remaining_binary, Rest}
    };
%% Rest of the message is having invalid lenght.
get_rest_bin(_, _, _) ->
    {error, invalid_remaining_length}.
