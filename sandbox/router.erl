%%   Copyright 2013 KuldeepSinh Chauhan
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

-module(router).
-export([get_type/1, get_remaining_bin/1, encode_l/1]).
-define(MAX_LENGTH, 268435455).

%% Identify message type.
get_type(<<1:4, _/binary>>) -> {ok, connect};
get_type(<<2:4, _/binary>>) -> {ok, connack};
get_type(<<3:4, _/binary>>) -> {ok, publish};
get_type(<<4:4, _/binary>>) -> {ok, puback};
get_type(<<5:4, _/binary>>) -> {ok, pubrec};
get_type(<<6:4, _/binary>>) -> {ok, pubrel};
get_type(<<7:4, _/binary>>) -> {ok, pubcomp};
get_type(<<8:4, _/binary>>) -> {ok, subscribe};
get_type(<<9:4, _/binary>>) -> {ok, suback};
get_type(<<10:4, _/binary>>) -> {ok, unsubscribe};
get_type(<<11:4, _/binary>>) -> {ok, unsuback};
get_type(<<12:4, _/binary>>) -> {ok, pingreq};
get_type(<<13:4, _/binary>>) -> {ok, pingresp};
get_type(<<14:4, _/binary>>) -> {ok, disconnect}.
%% Following function is not required according to protocol.
%% Let it crash.
%%get_type(_) -> {error, invalid_msg_type}.

%% Decode remaining length (RestBin does not contain FirstByte)
get_remaining_bin(RestBin) ->
    get_remaining_bin(RestBin, 0, 1).
get_remaining_bin(_, RLength, _)
  when (RLength > ?MAX_LENGTH) -> 
    {error, remaining_length_exceeds};
%% Calculate the remaining length value: 
%% Recurse if the value of the first bit is 1.
get_remaining_bin(<<1:1, Len:7, RestBin/binary>>, RLength, Multiplier) -> 
    get_remaining_bin(RestBin, RLength + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length : 
%% Return if the value of the first bit is 0.
get_remaining_bin(<<0:1, Len:7, RestBin/binary>>, RLength, Multiplier)   
    when ((RLength + Len * Multiplier) =:= size(RestBin)) ->
    {
      ok, RestBin 
      %% <ToDo : Remaining length helpful for debugging. I should log it's value (may be with 'lager').> 
      %% {remaining_length, RLength + Len * Multiplier}, 
      %% {remaining_binary, RestBin}      
    };
%% Rest of the message is having invalid lenght.
get_remaining_bin(_, _, _) -> 
    {error, invalid_remaining_length}.

%% Encode Length
encode_l(L) 
  when (L > ?MAX_LENGTH) ->
    {error, invalid_length};
encode_l(L) ->
    encode_l(<<>>, {L div 128, L rem 128}).

encode_l(Bin, {0, RBits}) ->
    list_to_binary([Bin, <<0:1, RBits:7>>]);
encode_l(Bin, {L, RBits}) ->
    encode_l(list_to_binary([Bin, <<1:1, RBits:7>>]), {L div 128, L rem 128}).
