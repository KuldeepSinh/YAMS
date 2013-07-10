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

-module(coner).
-export([chk_proto_name/1, extract_flags/1, extract_KAT_Payload/1, split_payload/1]).

%% If protocol name and protocol version are valid,
%% separate Flags and Payload from RestBin.
chk_proto_name(<<6:16, "MQIsdp", 3:8, Rest/binary>>) ->
    {ok, Rest};
chk_proto_name(<<_:16, _:48, 3:8, _/binary>>) ->
    {error, invalid_proto_name};
chk_proto_name(<<_16, _:48, Ver:8, _/binary>>) 
  when(Ver =/= 3)->
    {error, invalid_version}.

%% Extract connection flags from RestBin.
extract_flags(<<Usr:1, Pwd:1, WillR:1, WillQ:2, Will:1, ClnS:1, Rsvd:1, Rest/binary>>)->
    {ok, {con_flags, Usr, Pwd, WillR, WillQ, Will, ClnS, Rsvd}, Rest}.

%% extract Keep alive timer and Payload
extract_KAT_Payload(<<KAT:16, Payload/binary>>) ->
    {ok, {kat, KAT}, {payload, Payload}}.

split_payload(<<>>) ->
    [];
split_payload(<<L:16, Rest/binary>>)
    when ((L >= 0) and (size(Rest) >= L)) ->
    {Extract, RestBin} = split_binary(Rest, L),
    split_payload(RestBin) ++ [{L, Extract}];
split_payload(_) ->
    {error, length_string_mismatch}.



    
