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
%%%     This module will supervise connect message handlers.
%%% @end
%%% Created : 10 Aug 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(conn_f_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0
	]).

%% Supervisor callbacks
-export([
	 init/1
	]).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,

    %Suprevisor for handling flow for connect front-end
    ConnFHndlrSup = {conn_f_hndlr_sup, {conn_f_hndlr_sup, start_link, []}, Restart, Shutdown, Type, [conn_f_hndlr_sup]},
    %% Payload parser supervisor
    ConnPLPrsrSup = {conn_pl_prsr_sup,  {conn_pl_prsr_sup, start_link, []}, Restart, Shutdown, Type, [conn_pl_prsr_sup]},
    %% Variable Header parser supervisor
    ConnVHPrsrSup = {conn_vh_prsr_sup, {conn_vh_prsr_sup, start_link, []}, Restart, Shutdown, Type, [conn_vh_prsr_sup]},
    {ok, {SupFlags, [ConnFHndlrSup, ConnPLPrsrSup, ConnVHPrsrSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
