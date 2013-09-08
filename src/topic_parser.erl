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
%%%     This module will parse topics.
%%% @end
%%% Created :  4 Sep 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(topic_parser).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
	 create/0]).

%% gen_fsm callbacks
-export([init/1, 
	 ready/2, ready/3, 
	 hash/2, hash/3,
	 plus/2, plus/3,
	 slash/2, slash/3,
	 char/2, char/3,
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4]).

%% Event raiser
-export([send_event/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
send_event(Pid, {char_received, Char}) ->
    gen_fsm:sync_send_event(Pid, {char_received, Char});
%% send_all_state_event will be sent to any state which is currently active.
%% IOW, stop event will be send to the FSM, irrespective of its current state.
%% The stop event is an async event.
send_event(Pid, stop) ->
    gen_fsm:send_all_state_event(Pid, stop).

%% Create a new FSM to validate a topic.    
create() ->
    topic_parser_sup:start_child().

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, ready, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
ready(_Event, State) ->
    {next_state, ready, State}.

hash(_Event, State) ->
    {next_state, hash, State}.

plus(_Event, State) ->
    {next_state, plus, State}.

slash(_Event, State) ->
    {next_state, slash, State}.

char(_Event, State) ->
    {next_state, char, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%% handle characters when FSM is ready.
ready({char_received, $#}, _From, State) ->
    {reply, valid, hash, State};
ready({char_received, $+}, _From, State) ->
    {reply, valid, plus, State};
ready({char_received, $/}, _From, State) ->
    {reply, valid, slash, State};
ready({char_received, $\0}, _From, State) ->
    {stop, normal, {error, invalid_character}, State};
ready({char_received, _Char}, _From, State) ->
    {reply, valid, char, State}.

%% handle multi-level topic wild-card
hash({char_received, _Char}, _From, State) ->
    {stop, normal, {error, invalid_character}, State}.

%% handle single-level topic wild-card
plus({char_received, $/}, _From, State) ->
    {reply, valid, slash, State};
plus({char_received, _Char}, _From, State) ->
    {stop, normal, {error, invalid_character}, State}.

%% handle topic separator
slash({char_received, $#}, _From, State) ->
    {reply, valid, hash, State};
slash({char_received, $+}, _From, State) ->
    {reply, valid, plus, State};
slash({char_received, $/}, _From, State) ->
    {reply, valid, slash, State};
slash({char_received, $\0}, _From, State) ->
    {stop, normal, {error, invalid_character}, State};
slash({char_received, _Char}, _From, State) ->
    {reply, valid, char, State}.

%% handle other chars.
char({char_received, $#}, _From, State) ->
    {stop, normal, {error, invalid_character}, State};
char({char_received, $+}, _From, State) ->
    {stop, normal, {error, invalid_character}, State};
char({char_received, $/}, _From, State) ->
    {reply, valid, slash, State};
char({char_received, $\0}, _From, State) ->
    {stop, normal, {error, invalid_character}, State};
char({char_received, _Char}, _From, State) ->
    {reply, valid, char, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%% Asynch process to handle stop event.
%% It a callback called by gen_fsm.
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
