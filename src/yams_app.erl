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
%%% @copyright (C) 2013
%%% @doc yams_app is the main application. 
%%%      It starts yams_db followed by yums_sup supervisor. 
%%%      While stopping yams_app, yams_db is stopped first.
%%% @end
%%% Created : Aug 2013 by KuldeepSinh Chauhan
%%%-------------------------------------------------------------------

-module(yams_app).

%% Application behavior
-behaviour(application).

%% Application callbacks
-export([
	 start/2, %% Start YAMS app.
	 stop/1 %% Stop YAMS app.
	]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc Start YAMS Application. 
%%      First step is to start yams_db, followed by yams_sup.
%% @end
%% @spec start(_,_) -> {'error',_} | {'ok',_}.
%% -------------------------------------------------------------------  
start(_StartType, _StartArgs) ->
    %% Start database.
    yams_db:start(),
    %% Start root supervisor.
    case yams_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

%% -------------------------------------------------------------------
%% @doc First stop yams_db and then Stop YAMS application.
%% @end
%% @spec spec stop(_) -> 'ok'.
%% -------------------------------------------------------------------  
stop(_State) ->
    yams_db:stop(),
    ok.
