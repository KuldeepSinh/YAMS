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
%%%     This module opens the acceptor-socket for incomming client messages.
%%% @end
%%% Created :  8 Aug 2013 by KuldeepSinh Chauhan
%%% Updated : 05 Feb 2015 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(acceptor).

-behaviour(gen_event).

%% API
-export([
	 create/1, %% Call supervisor to create child process, that is the acceptor process.
	 start_link/1 %% Call gen_server to initialize acceptor process.
	]).

%% gen_event callbacks
-export([
	 init/1, 
	 handle_event/2, 
	 handle_call/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3
	]).

-define(SERVER, ?MODULE).

-record(state, 
	{
	  lsock, %% Listening socket.
	  asock, %% Acceptor socket.
	  apid %% PID of the acceptor. 
	}
       ).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Ask supervisor of the acceptor to create an acceptor process.
%% @end
%%--------------------------------------------------------------------
create(LSock) ->
    acceptor_sup:start_child(LSock).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% Acceptor supervisor will call this function, to create acceptor.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->   
    {ok, #state{lsock = LSock, apid = self()}, 0}.  

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{lsock = LSock} = State) ->
    %% accept client connection
    {ok, ASock} = gen_tcp:accept(LSock),
    %% make ASock ready to accept first messages
    inet:setopts(ASock, [{active, once}]),
    %% create a new acceptor
    create(LSock),	
    {noreply,State#state{asock = ASock}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
%% %%%     This module opens the acceptor-socket for incomming client messages.
%% %%% @end
%% %%% Created :  8 Aug 2013 by  KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
%% -module(acceptor).

%% -behaviour(gen_server).

%% %% API
%% -export([create/1, %% Call supervisor to create child process, that is the acceptor process.
%% 	 start_link/1, %% Call gen_server to initialize acceptor process.
%% 	 connack/2, %% Reply in response to the CONNECT message.
%% 	 suback/2
%% 	]).

%% %% gen_server callbacks
%% -export([init/1, 
%% 	 handle_call/3, 
%% 	 handle_cast/2, 
%% 	 handle_info/2,
%% 	 terminate/2, 
%% 	 code_change/3]).

%% -define(SERVER, ?MODULE). 

%% -record(state, {lsock, %% Listening socket.
%% 		asock, %% Acceptor socket.
%% 		apid, %% PID of the acceptor. 
%% 		status, %% Status of the client. (connected/undefined)
%% 		clientID, %% ID of the connected client.
%% 		kat %% Value of the Keep Alive Timer.
%% 	       }).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Ask supervisor of the acceptor to create an acceptor process.
%% %% @end
%% %%--------------------------------------------------------------------
%% create(LSock) ->
%%     acceptor_sup:start_child(LSock).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server
%% %% Acceptor supervisor will call this function, to create acceptor.
%% %%
%% %% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link(LSock) ->
%%     gen_server:start_link(?MODULE, [LSock], []).
  
%% %%--------------------------------------------------------------------
%% %% @doc
%% %% connack back to the client.
%% %% @end
%% %%--------------------------------------------------------------------
%% connack(APid, Msg) ->
%%     gen_server:cast(APid, {connack, Msg}).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% suback back to the client.
%% %% @end
%% %%--------------------------------------------------------------------
%% suback(APid, Msg) ->
%%     gen_server:cast(APid, {suback, Msg}).

%% %%%===================================================================
%% %%% gen_server callbacks
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Initializes the server (acceptor process)
%% %%
%% %% @spec init(Args) -> {ok, State} |
%% %%                     {ok, State, Timeout} |
%% %%                     ignore |
%% %%                     {stop, Reason}
%% %% @end
%% %%--------------------------------------------------------------------
%% init([LSock]) ->   
%%     {ok, #state{lsock = LSock, apid = self()}, 0}.  

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
%% handle_cast({connack, {ClientID, KAT, {ok, Connack}}}, #state{asock = ASock, apid = APid} = State) ->
%%     %% Add a record into the cid_to_apid table.
%%     client_to_apid:insert(ClientID, APid),
%%     %% Create a new state
%%     NewState = State#state{status = connected, clientID = ClientID, kat = KAT},
%%     gen_tcp:send(ASock, Connack),
%%     {noreply, NewState};
%% handle_cast({connack, {error, Connack}}, #state{asock = ASock} = State) ->
%%     gen_tcp:send(ASock, Connack),
%%     {noreply, State};
%% handle_cast({suback, SubAck}, #state{asock = ASock} = State) ->
%%     %%Send suback
%%     gen_tcp:send(ASock, SubAck),
%%     {noreply, State};
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
%% handle_info(timeout, #state{lsock = LSock} = State) ->
%%     %% accept client connection
%%     {ok, ASock} = gen_tcp:accept(LSock),
%%     %% make ASock ready to accept first messages
%%     inet:setopts(ASock, [{active, once}]),
%%     %% create a new acceptor
%%     create(LSock),	
%%     {noreply,State#state{asock = ASock}};
%% %% Accept message from the client.
%% handle_info({tcp, ASock, Msg}, #state{apid = APid, status = Status, clientID = ClientID} = State) ->
%%     %% create a new router and send the message to it,
%%     %% along with the status and the ID of the client.
%%     router:create(APid, Status, ClientID, Msg),
%%     %% make ASock ready to accept next messages
%%     inet:setopts(ASock, [{active, once}]),
%%     {noreply, State};
%% %% Close the acceptor (socket).
%% handle_info({tcp_closed, _ASock}, State) ->
%%     {stop, normal, State};
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
