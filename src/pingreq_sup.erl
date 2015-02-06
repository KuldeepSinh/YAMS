%%%-------------------------------------------------------------------
%%% @author KuldeepSinh Chauhan
%%% @copyright (C) 2015, KuldeepSinh Chauhan
%%% @doc
%%%    This module will supervise process handling ping-requests
%%% @end
%%% Created :  6 Feb 2015 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(pingreq_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    supervisor:start_link({local, ?MODULE},?MODULE, []).

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

    PingRequest = {pingreq, {pingreq, start_link, []}, Restart, Shutdown, Type, [pingreq]},
    {ok, {SupFlags, [PingRequest]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
