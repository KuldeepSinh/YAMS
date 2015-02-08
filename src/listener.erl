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
%%% @author KuldeepSinh Chauhan
%%% @copyright (C) 2013,
%%% @doc This module opens TCP listener socket for incoming client connections.
%%% @end
%%% Created : 08 Aug 2013 by KuldeepSinh Chauhan
%%% Updated : 06 Feb 2015 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(listener).

-behaviour(gen_server).

 -export([
	  start_link/0, %% Start listener (server) on the default port # 8789
	  start_link/1, %% Start listener (server) on the port passed by the client.
	  stop/0 %% Stop the server.
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
 -define(DEFAULT_PORT, 8789).
 -record(state, {port, lsock}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server using default port = 8789
%%
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server on the port given by the user.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

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
init([Port]) ->
    % start a TCP listener.
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}]),
    % In following function calls, 3rd argument = 0 fires timeout, 
    % which will be handled by function handle_info/2
    {ok, #state{port=Port, lsock=LSock}, 0}.

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
handle_info(timeout, #state{lsock = LSock} = State) ->
    %%create an acceptor
    acceptor:create(LSock),
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
%% %%% @author KuldeepSinh Chauhan
%% %%% @copyright (C) 2013,
%% %%% @doc This module opens TCP listener socket for incoming client connections.
%% %%% @end
%% %%% Created : 8 Aug 2013 by KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
%% -module(listener).
%% %% listener is a gen_server. 
%% -behaviour(gen_server).

%% %% API
%% -export([start_link/0, %% Start listener (server) on the default port # 8789
%% 	 start_link/1, %% Start listener (server) on the port passed by the client.
%% 	 stop/0 %% Stop the server.
%% 	]).

%% %% gen_server callbacks
%% -export([init/1, 
%% 	 handle_call/3, 
%% 	 handle_cast/2, 
%% 	 handle_info/2,
%% 	 terminate/2, 
%% 	 code_change/3]).

%% -define(SERVER, ?MODULE).
%% -define(DEFAULT_PORT, 8789).
%% -record(state, {port, lsock}).

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server using default port = 8789
%% %%
%% %% @spec start_link() -> {ok, Pid}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link() ->
%%     start_link(?DEFAULT_PORT).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server on the port given by the user.
%% %%
%% %% @spec start_link(Port::integer()) -> {ok, Pid}
%% %% where
%% %% Pid = pid()
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link(Port) ->
%%     gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Stops the server
%% %%
%% %% @spec stop() -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% stop() ->
%%     gen_server:cast(?SERVER, stop).

%% %%%===================================================================
%% %%% gen_server callbacks
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Initializes the server
%% %%
%% %% @spec init(Args) -> {ok, State} |
%% %% {ok, State, Timeout} |
%% %% ignore |
%% %% {stop, Reason}
%% %% @end
%% %%--------------------------------------------------------------------
%% init([Port]) ->
%%     % start a TCP listener.
%%     {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}]),
%%     % In following function calls, 3rd argument = 0 fires timeout, 
%%     % which will be handled by function handle_info/2
%%     {ok, #state{port=Port, lsock=LSock}, 0}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling call messages
%% %%
%% %% @spec handle_call(Request, From, State) ->
%% %% {reply, Reply, State} |
%% %% {reply, Reply, State, Timeout} |
%% %% {noreply, State} |
%% %% {noreply, State, Timeout} |
%% %% {stop, Reason, Reply, State} |
%% %% {stop, Reason, State}
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
%% %% {noreply, State, Timeout} |
%% %% {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_cast(stop, State) ->
%%     {stop, normal, State}.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Handling all non call/cast messages
%% %%
%% %% @spec handle_info(Info, State) -> {noreply, State} |
%% %% {noreply, State, Timeout} |
%% %% {stop, Reason, State}
%% %% @end
%% %%--------------------------------------------------------------------
%% handle_info(timeout, #state{lsock = LSock} = State) ->
%%     %%create a pool of acceptors
%%     create_acceptor_pool(LSock, 10),
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
%% create_acceptor_pool(LSock, Count) ->
%%     [acceptor:create(LSock) || _ <- lists:seq(1, Count)],
%%     ok.
