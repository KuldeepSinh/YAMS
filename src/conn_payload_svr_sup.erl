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
%%%     This module will supervise connect payload gen server.
%%% @end
%%% Created : 13 Feb 2015 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(conn_payload_svr_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0, 
	 start_child/2
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(APid, Msg) ->
    supervisor:start_child(?SERVER, [APid, Msg]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    %% conn_paylaod_svr is a gen_server, which will split payload in accordance with the connect_flags. 
    %% Further it will group together payload components along with the connect_flags associated with them.
    ConnPayloadSvr = {conn_payload_svr, {conn_payload_svr, start_link, []}, Restart, Shutdown, Type, [conn_payload_svr]},
    {ok, {SupFlags, [ConnPayloadSvr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
