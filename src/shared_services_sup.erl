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
%%%     tcp_sup : module to listen and accept messages from/to clients connected over TCP.
%%% @end
%%% Created : 10 Aug 2013 by KuldeepSinh Chauhan
%%% Updated : 05 Feb 2015 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(shared_services_sup).

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

    %Supervisor for Packet router
    RouterSup = {router_sup, {router_sup, start_link, []}, Restart, Shutdown, Type, [router_sup]},

    %Supervisor for Session server
    SessionSup = {session_sup, {session_sup, start_link, []}, Restart, Shutdown, Type, [session_sup]},

    %Supervisor for Will server
    WillSup = {will_sup, {will_sup, start_link, []}, Restart, Shutdown, Type, [will_sup]},

    %Supervisor for Keep alive time server
    KatSup = {kat_sup, {kat_sup, start_link, []}, Restart, Shutdown, Type, [kat_sup]},


    %%In the following list, the order of given supervisors is very important.
    %%Starting from the lowest level, supervisors will be started upto the highest level, 
    %%ensuring, lower level Supervisor is started before it is used by the higher level.
    {ok, 
     {SupFlags, 
      [
       WillSup,
       KatSup,
       SessionSup,       
       RouterSup
      ]
     }
    }.

%% ===================================================================
%% Non-API functions
%% ===================================================================
