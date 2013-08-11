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

-module(yams_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,

    LisSup = {lis_sup, {lis_sup, start_link, []}, Restart, Shutdown, Type, [lis_sup]},
    AccSup = {acc_sup, {acc_sup, start_link, []}, Restart, Shutdown, Type, [acc_sup]},

    %%Here, order of given supervisors is very important.
    %%AccSup is deliberately ordered prior to LisSup,
    %%Reason: acceptor sockets are created from listener sockets.
    %%so, the AccSup should be ready before LisSup.
    {ok, {SupFlags, [AccSup, LisSup]}}.

%% ===================================================================
%% Non-API functions
%% ===================================================================
