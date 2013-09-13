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

-include("../include/yams_db.hrl").

%% "subscription" is a disc based table, which stores 
%% the mapping between Client ID and associated Topics, their QoS and Message ID.
insert({ClientID, MsgID, Topic, Dup, QoS, Retain, Msg, Is_Retained}) ->
    mnesia:dirty_write(#publication
		       {
			 cid = ClientID,
			 msgID = MsgID, 
			 topic = Topic, 
			 dup = Dup, 
			 qos = QoS, 
			 retain = Retain, 
			 msg = Msg, 
			 is_retained = Is_Retained
		       }),
    {ok, publication_saved}.

%% This fucntion will search the "subscription" table based on the given Client ID.
lookup(MsgID) ->
    mnesia:dirty_read(publication, MsgID).
