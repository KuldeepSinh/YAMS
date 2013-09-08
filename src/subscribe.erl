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
%%%
%%% @end
%%% Created : 29 Aug 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(subscribe).

-behaviour(gen_server).

%% API
-export([start_link/3,
	 create/3, 
	 stop/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {apid, dup, msg, self, msgID, subscriptions = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(APid, Dup, Msg) ->
    gen_server:start_link(?MODULE, [APid, Dup, Msg], []).

create(APid, Dup, Msg) ->
    subscribe_sup:start_child(APid, Dup, Msg).

stop(SPid) ->
    gen_server:call(SPid, stop).

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
init([Apid, Dup, Msg]) ->
    {ok, #state{apid = Apid, dup = Dup, msg = Msg, self = self()}, 0}.

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
handle_info(timeout, #state{self = SPid} = State) ->
    process_message(State),   
    stop(SPid),
    {noreply, State};
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
process_message(#state{msg = <<ID:16, Payload/binary>>} = State)  ->
    NewState = State#state{msgID = ID},
    split_payload(Payload, NewState).

%% Split payload into topics.
split_payload(<<>>, #state{subscriptions = Subscriptions} = State) -> 
    NewState = State#state{subscriptions = lists:reverse(Subscriptions)},
    validate_qos(NewState),
    case (NewState#state.subscriptions) of
	[] ->
	    {error, empty_subscriptions_list};
	_ ->
	    validate_topics(NewState#state.subscriptions, [])
    end;
split_payload(<<L:16, Rest/binary>>,  #state{subscriptions = Subscriptions} = State)
  when(size(Rest) >= (L + 1)) ->
    {Topic, RB} = split_binary(Rest, L),
    <<QoS:8, RestBin/binary>> = RB,
    NewState = State#state{subscriptions = [{Topic, QoS}] ++ Subscriptions},
    split_payload(RestBin, NewState);
split_payload(_, _) ->
    {error, length_mismatch}.

%% QoS validation.
validate_qos([]) ->
    {ok, valid_qos};
validate_qos(#state{subscriptions = [{_Topic, 0}| T]}) ->
    validate_qos(T);
validate_qos(#state{subscriptions = [{_Topic, 1}| T]}) ->
    validate_qos(T);
validate_qos(#state{subscriptions = [{_Topic, 2}| T]}) ->
    validate_qos(T);
validate_qos(_) ->
    {error, invalid_qos}.

%% Validate subscriptions
validate_topics([], FSM_IDs) ->
    {ok, all_topics_valid, FSM_IDs};
validate_topics([{[], _} |  _], _FSM_IDs) ->
    {error, empty_topic};
validate_topics([{Topic, _} | T], FSM_IDs) -> 
    TopicString = binary_to_list(Topic),
    {ok, Pid} = topic_parser:create(),
    case parse_topic(Pid, TopicString) of 
	{ok, valid} ->
	    topic_parser:send_event(Pid, stop),
	    validate_topics(T, [Pid] ++ FSM_IDs);
	_  ->
	    {error, invalid_topic}
    end.

%% parse topic. (call FSM).
parse_topic(_Pid, []) ->
    {ok, valid};
parse_topic(Pid, [H|T]) ->
    case (topic_parser:send_event(Pid, {char_received, H})) of 
	valid ->
	    parse_topic(Pid, T);
	_  ->
	    error
    end.
