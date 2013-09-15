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
%%%     This module is intended to perform CRUD operations for subsriptions.
%%% @end
%%% Created :  11 Sep 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------

-module(subscriptions).
-export([insert/1, %% Insert topic.
	 lookup/1 %% Lookup topic.
	]).
-include_lib("stdlib/include/qlc.hrl").
-include("../include/yams_db.hrl").


%% "subscription" is a disc based table, which stores 
%% the mapping between Client ID and associated Topics, their QoS and Message ID.
insert(Subscriptions) ->
    F = fun() ->
		[mnesia:write(#subscription{cid = Cid, topic = Topic, qos = QoS, msgID = MsgID}) || {Cid, Topic, QoS, MsgID} <- Subscriptions]
	end,
    mnesia:transaction(F),
    {ok, topics_susbscribed}.

%% This fucntion will search the "subscription" table based on the given Client ID.
lookup({cid, Cid}) ->
    Query = qlc:q([Subscription || Subscription <- mnesia:table(subscription), Subscription#subscription.cid =:= Cid]),
    yams_db:execute(Query);
%% This fucntion will search the "subscription" table based on the given topic.
lookup({topic, Topic}) ->
    Query = qlc:q([Subscription || Subscription <- mnesia:table(subscription), Subscription#subscription.topic =:= Topic]),
    yams_db:execute(Query).
