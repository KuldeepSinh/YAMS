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
-export([insert/2,
	 lookup/1,
	 delete/1]).

-include("../include/yams_db.hrl").

%% "cid_to_apid" is a memory based table, which stores 
%% the mapping between Client ID and associated Acceptor Pid.
insert(Cid, APid) ->
    mnesia:dirty_write(#cid_to_apid{cid = Cid, apid = APid}).

%% This fucntion will search the "cid_to_apid" table based on the given Client ID.
lookup(Cid) ->
    case mnesia:dirty_read(cid_to_apid, Cid) of
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

%% Delete cid_to_apid mapping based on passed Cid.
delete(Cid) ->
    case mnesia:dirty_index_read(cid_to_apid, Cid, #cid_to_apid.cid) of
	[#cid_to_apid{} = Record] ->
	    mnesia:dirty_delete_object(Record);
	_ ->
	    ok
    end.
