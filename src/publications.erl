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
%%%     This module is intended to perform CRUD operations for publications.
%%% @end
%%% Created :  13 Sep 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(publications).

-export([insert/1, %% Insert topic.
	 lookup/1 %% Lookup topic.
	]).

-include_lib("stdlib/include/qlc.hrl").

-include("../include/yams_db.hrl").

%% "publication" is a disc based table.
insert({ClientID, MsgID, Topic, Dup, QoS, Retain, Msg, Is_Retained}) ->
    F = fun() ->
		mnesia:write(#publication
			     {
			       cid = ClientID,
			       msgID = MsgID, 
			       topic = Topic, 
			       dup = Dup, 
			       qos = QoS, 
			       retain = Retain, 
			       msg = Msg, 
			       is_retained = Is_Retained
			     })
	end,
    mnesia:transaction(F),
    {ok, publication_saved}.

%% This fucntion will search the "publication" table based on the given Message ID.
lookup(MsgID) ->
    Query = qlc:q([Publication || Publication <- mnesia:table(publication), Publication#publication.msgID =:= MsgID]),
    yams_db:execute(Query).
