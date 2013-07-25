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
%% Process variable header
-export([validate_proto_name/1, get_flags/1, get_kat_payload/1]).
%% Process payload
-export([split_payload/1, validate_client/1, get_wills/1, get_user/3, get_pswd/4]).
%% CONNACK
-export([connack/1]).

%% Validate if protocol name and protocol version are valid.
validate_proto_name(<<6:16, "MQIsdp", 3:8, Rest/binary>>) -> {ok, Rest};
validate_proto_name(<<_:16, _:48, 3:8, _/binary>>) -> {error, invalid_proto_name};
validate_proto_name(<<_16, _:48, Ver:8, _/binary>>) when(Ver =/= 3)-> {error, invalid_version}.

%% Extract connection flags from RestBin.
get_flags(<<Usr:1, Pwd:1, WillR:1, WillQ:2, Will:1, ClnS:1, Rsvd:1, Rest/binary>>) -> 
    {
      ok, 
      {con_flags, Usr, Pwd, WillR, WillQ, Will, ClnS, Rsvd}, 
      Rest
    }.

%% extract Keep alive timer and Payload from RestBin
get_kat_payload(<<KAT:16, Payload/binary>>) ->
    {
      ok, 
      {kat, KAT}, 
      {payload, Payload}
    }.

%% Split payload into the list of {FieldLength, Field}
%% Note: caller should reverse the list returned from this function.
split_payload(<<>>) -> 
    [];
split_payload(<<L:16, Rest/binary>>)
    when (size(Rest) >= L) ->
    {Extract, RestBin} = split_binary(Rest, L),
    split_payload(RestBin) ++ [{L, Extract}];
split_payload(_) -> 
    {error, length_mismatch}.

%% Validate client identifier
validate_client({L, Val})
  when((L >= 1) and (L =< 23)) ->
    %%<ToDo> : Lookup in the client-registry if the client ID is unique/client is registered with the system.
    {ok, valid_client, binary_to_list(Val)};
validate_client(_) -> 
    {error, invalid_client}.

%% Assumption#1: This function is called only when Will == 1.
%% Assumption#2: Paylod_list contains will topic and will message both.
%% Extract Will Topic and Will Message
get_wills(Payload_list) ->
    Topic = lists:nth(2, Payload_list),
    Msg = lists:nth(3, Payload_list),
    {ok, Topic, Msg}.    

%% arguments = (Will, User, Payload_list)
get_user(1, 1, Payload_list) 
  when(erlang:length(Payload_list) >= 4) ->
    User = lists:nth(4, Payload_list),
    {ok, User};    
get_user(0, 1, Payload_list) 
  when(erlang:length(Payload_list) >= 2) ->
    User = lists:nth(2, Payload_list),
    {ok, User}.

%% arguments = (Will, User, Password, Payload_list)
get_pswd(1, 1, 1, Payload_list) 
  when(erlang:length(Payload_list) == 5) ->
    Pswd = lists:nth(5, Payload_list),
    {ok, Pswd};    
get_pswd(0, 1, 1, Payload_list) 
  when(erlang:length(Payload_list) == 3) ->
    Pswd = lists:nth(3, Payload_list),
    {ok, Pswd}.


%% CONNACK = Acknowledge CONNECT
connack(0) -> {ok, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, 0:8>>};
connack(Code) -> {error, <<2:4, 0:1, 0:2, 0:1, 2:8, 0:8, Code:8>>}.
