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
-export([
	 start_link/4,
	 create/4,
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
-define(MAX_LENGTH, 268435455).

-record(state, 
	{
	  apid, %% PID of associated Acceptor.
	  clientID, %% Client ID of associated Client.
	  msg, %% Message received from the acceptor.
	  self, %% PID of self (RPid).
	  status %% status of the acceptor (connected/undefined).
	}
       ).

%%%===================================================================
%%% API
%%%===================================================================
create(APid, Status, ClientID, Msg) ->
    router_sup:start_child(APid, Status, ClientID, Msg).

%%--------------------------------------------------------------------
%% @doc
%% Starts the router
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(APid, Status, ClientID, Msg) ->
    gen_server:start_link(?MODULE, [APid, Status, ClientID, Msg], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the router
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

stop(RPid) ->
    gen_server:cast(RPid, stop).


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
init([APid, Status, ClientID, Msg]) ->
    {ok, #state{apid = APid, status = Status, clientID = ClientID, msg = Msg, self = self()}, 0}.

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
handle_info(timeout, #state{apid = APid, status = Status, clientID = ClientID, msg = Msg, self = RPid} = State) ->
    %{ok, _Type} = route(APid, Status, ClientID, Msg),
    stop(RPid),
    {noreply, State};
%% handle_info(timeout, _State) ->
%%     stop(self());
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
%% %%%     This module will route messages to message handlers in accordance with their type.
%% %%% @end
%% %%% Created : 15 Aug 2013 by KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
%% -module(router).

%% -behaviour(gen_server).

%% %% API
%% -export([start_link/4,
%% 	 create/4,
%% 	 stop/1]).

%% %% gen_server callbacks
%% -export([init/1, 
%% 	 handle_call/3, 
%% 	 handle_cast/2, 
%% 	 handle_info/2,
%% 	 terminate/2, 
%% 	 code_change/3]).

%% -define(SERVER, ?MODULE). 
%% -define(MAX_LENGTH, 268435455).

%% -record(state, {apid, %% PID of associated Acceptor.
%% 		clientID, %% Client ID of associated Client.
%% 		msg, %% Message received from the acceptor.
%% 		self, %% PID of self (RPid).
%% 		status %% status of the acceptor (connected/undefined).
%% 	       }).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% create(APid, Status, ClientID, Msg) ->
%%     router_sup:start_child(APid, Status, ClientID, Msg).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server
%% %%
%% %% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link(APid, Status, ClientID, Msg) ->
%%     gen_server:start_link(?MODULE, [APid, Status, ClientID, Msg], []).

%% stop(RPid) ->
%%     gen_server:call(RPid, stop).
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
%% init([APid, Status, ClientID, Msg]) ->
%%     {ok, #state{apid = APid, status = Status, clientID = ClientID, msg = Msg, self = self()}, 0}.

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
%% handle_cast(_Msg, State) ->
%%     {noreply, State}.

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
%% handle_info(timeout, #state{apid = APid, status = Status, clientID = ClientID, msg = Msg, self = RPid} = State) ->
%%     {ok, _Type} = route(APid, Status, ClientID, Msg),
%%     stop(RPid),
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
%% %% Identify message type.
%% route(APid, _Status, _ClientID, <<1:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%%     {ok, RestMsg} = get_rest_bin(Rest), 
%%     connect:create(APid, RestMsg),
%%     {ok, connect};
%% %%route(APid, connected, <<2:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, connack};
%% route(APid, connected, ClientID, <<3:4, Dup:1, QoS:2, Retain:1, Rest/binary>>) -> 
%%     {ok, RestMsg} = get_rest_bin(Rest),
%%     publish:create(APid, ClientID, Dup, QoS, Retain, RestMsg),
%%     {ok, publish};
%% %%route(APid, connected, <<4:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest),     
%% %%    {ok, puback};
%% %%route(APid, connected, <<5:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest),  
%% %%    {ok, pubrec};
%% %%route(APid, connected, <<6:4, Dup:1, QoS:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, pubrel};
%% %%route(APid, connected, <<7:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, pubcomp};
%% %% Last 3 bits of the First-byte are now called reserved bits in the next version.
%% %% The required value for last 3 bits is 2 (which is same as required for the current version 4.0).
%% %% That's why the value of the QoS field (second and third last bits) is set to 1.
%% route(APid, connected, ClientID, <<8:4, Dup:1, 1:2, 0:1, Rest/binary>>) -> 
%%     {ok, RestMsg} = get_rest_bin(Rest), 
%%     subscribe:create(APid, ClientID, Dup, RestMsg),
%%     {ok, subscribe}.
%% %%route(APid, connected, <<9:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, suback};
%% %%route(APid, connected, <<10:4, Dup:1, QoS:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, unsubscribe};
%% %%route(APid, connected, <<11:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, unsuback};
%% %%route(APid, connected, <<12:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, pingreq};
%% %%route(APid, connected, <<13:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, pingresp};
%% %%route(APid, connected, <<14:4, 0:1, 0:2, 0:1, Rest/binary>>) -> 
%% %%    {ok, RestMsg} = get_rest_bin(Rest), 
%% %%    {ok, disconnect}.

%% %%================================
%% %% Decode remaining length (RestBin does not contain FirstByte)
%% get_rest_bin(Rest) ->
%%     get_rest_bin(Rest, 0, 1).
%% get_rest_bin(_, RLength, _)
%%   when (RLength > ?MAX_LENGTH) ->
%%     {error, remaining_length_exceeds};
%% %% Calculate the remaining length value:
%% %% Recurse if the value of the first bit is 1.
%% get_rest_bin(<<1:1, Len:7, Rest/binary>>, RLength, Multiplier) ->
%%     get_rest_bin(Rest, RLength + Len * Multiplier, Multiplier * 128);
%% %% Calculate Value of the remaining length :
%% %% Return if the value of the first bit is 0.
%% get_rest_bin(<<0:1, Len:7, Rest/binary>>, RLength, Multiplier)
%%     when ((RLength + Len * Multiplier) =:= size(Rest)) ->
%%     {
%%       ok, Rest
%%       %% <ToDo : Remaining length helpful for debugging. I should log it's value (may be with 'lager').>
%%       %% {remaining_length, RLength + Len * Multiplier},
%%       %% {remaining_binary, Rest}
%%     };
%% %% Rest of the message is having invalid lenght.
%% get_rest_bin(_, _, _) ->
%%     {error, invalid_remaining_length}.
