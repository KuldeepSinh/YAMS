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
%%%     This module opens the acceptor-socket for incomming client messages.
%%% @end
%%% Created :  22 Aug 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------

-module(client_to_apid).
-export([insert/2, %% Insert mapping
	 delete/1, %% Delete based on Cid
	 lookup/1 %% Query based on Cid
	]).

-include_lib("stdlib/include/qlc.hrl").
-include("../include/yams_db.hrl").

%% "cid_to_apid" is a memory based table, which stores 
%% the mapping between Client ID and associated Acceptor Pid.
insert(Cid, APid) ->
    F = fun() ->
		mnesia:write(#cid_to_apid{cid = Cid, apid = APid})
	end,
    mnesia:transaction(F),
    {ok, record_added}.

%% Delete cid_to_apid mapping based on passed Cid.
delete(Cid) ->
    Oid = {cid_to_apid, Cid},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

%% This fucntion will search the "subscription" table based on the given Client ID.
lookup({cid, Cid}) ->
    Query = qlc:q([X || X <- mnesia:table(subscription), X#subscription.cid =:= Cid]),
    case yams_db:execute(Query) of
	[{cid_to_apid, Cid, APid}] ->
	    case is_pid_alive(APid) of	    
		true ->
		    {ok, APid};
		false ->
		    {error, not_found}
	    end;
	[] ->
	    {error, not_found}
    end.

%% Following function determines, if the given APid is still alive or not.
is_pid_alive(APid) when node(APid) =:= node() ->
    is_process_alive(APid);
is_pid_alive(APid) ->
    lists:member(node(APid), nodes()) andalso (rpc:call(node(APid), erlang, is_process_alive, [APid]) =:= true).
