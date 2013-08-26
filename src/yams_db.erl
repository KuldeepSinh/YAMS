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

-module(yams_db).
-export([init/0,
	 insert_acceptor/2,
	 lookup_acceptor/1,
	 delete_acceptor/1]).

-record(acceptor, {apid, cid}).
-record(subscription, {cid, topic}).

init() ->
    %% Start mnesia
    mnesia:start(),
    %% Client table will store mapping between ID of the connected client and its associated Acceptor Pid.
    mnesia:create_table(acceptor, [{index, [cid]}, {attributes, record_info(fields, acceptor)}]),
    %% Subscription table will store Client ID and its subscribed topics.
    mnesia:create_table(subscription, [{type, bag}, {attributes, record_info(fields, subscription)}]).

%% "Acceptor" is a memory based table, which stores 
%% pid of the acceoptor and the client ID it is connected with.
insert_acceptor(APid, Cid) ->
    mnesia:dirty_write(#acceptor{apid = APid, cid = Cid}).

%% This fucntion will search the "Acceptor" table based on the given Client ID.
lookup_acceptor(Cid) ->
    case mnesia:dirty_read(acceptor, Cid) of
	[{acceptor, APid, Cid}] ->
	    case is_pid_alive(APid) of	    
		true ->
		    {ok, APid};
		false ->
		    {error, not_found}
	    end;
	[] ->
	    {error, not_found}
    end.

%% Following function determines, if the given Pid is still alive or not.
is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    lists:member(node(Pid), nodes()) andalso (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).

%% Delete Acceptor porcess based on passed apid.
delete_acceptor(APid) ->
    case mnesia:dirty_index_read(acceptor, APid, #acceptor.apid) of
	[#acceptor{} = Record] ->
	    mnesia:dirty_delete_object(Record);
	_ ->
	    ok
    end.
