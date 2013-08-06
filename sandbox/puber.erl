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
-module(puber).
%% First Byte
-export([split_first_byte/1]).
%% Variable Header & Payload
-export([get_topic/1, get_id_payload/1]).
%% Response to the publisher
-export([puback/1, pubrec/1, pubrel/1, pubcomp/1]).

split_first_byte(<<T:4, Dup:1, QoS:2, Rest:1>>) ->
    {ok, T, Dup, QoS, Rest}.

get_topic(<<L:16, Rest/binary>>)
  when(size(Rest) >= L) ->
    split_binary(Rest, L);
get_topic(_) ->
    {error, length_mismatch}.

get_id_payload(<<Id:16, Payload/binary>>) ->
    {ok, Id, Payload}.

puback(ID) ->
    {ok, <<4:4, 0:1, 0:2, 0:1, 2:8, ID:16>>}.

pubrec(ID) ->
    {ok, <<5:4, 0:1, 0:2, 0:1, 2:8, ID:16>>}.

pubrel(ID) ->
    {ok, <<6:4, 0:1, 1:2, 0:1, 2:8, ID:16>>}.

pubcomp(ID) ->
    {ok, <<7:4, 0:1, 0:2, 0:1, 2:8, ID:16>>}.
