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
	 lookup/1, %% Lookup topic.
	 delete/1 %% Delete topic based on the Cid
	]).
-include_lib("stdlib/include/qlc.hrl").

-include("../include/yams_db.hrl").


%% "subscription" is a disc based table, which stores 
%% the mapping between Client ID and associated Topics, their QoS and Message ID.
insert(Subscriptions) ->
    F = fun() ->
		do_insertion(Subscriptions)
	end,
    mnesia:transaction(F),
    {ok, topics_susbscribed}.

%% Delete "subscription" based on passed Cid.
%% CAUTION : This will delete all the records with matching Cid.
delete(Cid) ->
    Oid = {subscription, Cid},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

%% This fucntion will search the "subscription" table based on the given Client ID.
lookup({cid, Cid}) ->
    Query = qlc:q([Subscription || Subscription <- mnesia:table(subscription), Subscription#subscription.cid =:= Cid]),
    yams_db:execute(Query);
%% This fucntion will search the "subscription" table based on the given topic.
lookup({topic, Topic}) ->
    Query = qlc:q([Subscription || Subscription <- mnesia:table(subscription), Subscription#subscription.topic =:= Topic]),
    yams_db:execute(Query).


%% Insert/Update each subscription.
do_insertion([]) ->
    ok;
do_insertion([{Cid, Topic, QoS, MsgID} | T]) ->
    %% Find duplicate subscriptions
    Result = find_duplicates(Cid, Topic),
    %% Delete all duplicate results
    delete_all(Result),
    %% Write new subscription in DB.
    mnesia:write(#subscription{cid = Cid, topic = Topic, qos = QoS, msgID = MsgID}),
    do_insertion(T).


%% Find duplicate subscriptions
find_duplicates(Cid, Topic) ->
    %%create query to find duplicates based on matching Cid and Topic
    Query = qlc:q([Subscription || Subscription <- mnesia:table(subscription), 
				   Subscription#subscription.cid =:= Cid,
				   Subscription#subscription.topic =:= Topic
		  ]),
    %%Execute a query and check if any duplicate received.
    qlc:e(Query).

%% Delete all subscriptions.
delete_all({error, _, _}) ->
    ok;
delete_all([]) ->
    ok;
delete_all([H | T]) ->
    mnesia:delete_object(subscription, H, write),
    delete_all(T).
