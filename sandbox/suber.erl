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

-module(suber).
%% Fixed header (will be used for subscirbe and unsubscribe both)
-export([split_fist_byte/1]).
%% Variable header (will be used for subscirbe and unsubscribe both)
-export([get_id_payload/1]).
%% Payload (will be used for subscirbe and unsubscribe both)
-export([split_payload/2]).
%% Response
-export([suback/3, unsuback/1]).

%% Argument = First Byte ofthe message
%% For subscribe
split_fist_byte(<<8:4, Dup:1, QoS:2, Retain:1>>) ->
    {ok, Dup, QoS, Retain};
%% For unsubscribe
split_fist_byte(<<10:4, Dup:1, QoS:2, Retain:1>>) ->
    {ok, Dup, QoS, Retain}.
    
get_id_payload(<<ID:16, Payload/binary>>) ->
    {ok, ID, Payload}.

%% Subscribe: Payload is a collection of triplets {Length, Topic, QoS}
%% guard is checked agains (L + 1), 
%% where 1 represents the byte required for QoS.
split_payload(_, <<>>) -> 
    [];
split_payload(subscribe, <<L:16, Rest/binary>>)
  when(size(Rest) >= (L + 1)) ->
    {Topic, RB} = split_binary(Rest, L),
    <<QoS:8, RestBin/binary>> = RB,
    split_payload(subscribe, RestBin) ++ [{L, Topic, QoS}];
%% Unsubscribe: Payload is a collection of doublets {Length, Topic}
split_payload(unsubscribe, <<L:16, Rest/binary>>)
  when(size(Rest) >= L) ->
    {Topic, RestBin} = split_binary(Rest, L),
    split_payload(unsubscribe, RestBin) ++ [{L, Topic}];
split_payload(_, _) ->
    {error, length_mismatch}.

%% Response suback
suback(RemainingLength, MsgID, Qs) ->
    <<9:4, 0:1, 0:2, 0:1, RemainingLength/binary, MsgID:16, Qs/binary>>.
    
%% Response unsuback
unsuback(MsgID) ->
    <<11:4, 0:1, 0:2, 0:1, 2:8, MsgID:16>>.
