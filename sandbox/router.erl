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
-export([chk_type/1, decode_l/1, encode_l/1, route/1]).
-define(MAX_LENGTH, 268435455).
-include_lib("eunit/include/eunit.hrl").

%% Validate message_type
chk_type(Bin = <<MT:4, _/binary>>) 
  when ((MT <1) or (MT > 14)) -> {error, invalid_msg_type, Bin};
chk_type(Bin) -> {ok, valid_msg_type, Bin}.

%% EUnit tests
chk_type_less_than_1_test() ->
    ?assert({error, invalid_msg_type, <<0:4, 23400>>} =:= chk_type(<<0:4, 23400>>)).
chk_type_more_than_14_test() ->
    ?assert({error, invalid_msg_type, <<15:4, 23400>>} =:= chk_type(<<15:4, 23400>>)).
chk_type_equal_1_test() ->
    ?assert({ok, valid_msg_type, <<1:4, 23400>>} =:= chk_type(<<1:4, 23400>>)).
chk_type_equal_14_test() ->
    ?assert({ok, valid_msg_type, <<14:4, 23400>>} =:= chk_type(<<14:4, 23400>>)).
chk_type_equal_7_test() ->
    ?assert({ok, valid_msg_type, <<7:4, 23400>>} =:= chk_type(<<7:4, 23400>>)).

%% Validate remaining length
decode_l(<<Fst_Byte:8, RestBin/binary>>) ->
    decode_l(Fst_Byte, RestBin, 0, 1).
decode_l(_, _, RL, _)
  when (RL > ?MAX_LENGTH) -> 
    {error, remaining_length_exceeds};
%% Calculate the remaining length value: 
%% Recurse if the value of the first bit is 1.
decode_l(Fst_Byte, <<1:1, Len:7, Rest/binary>>, RL, Multiplier) -> 
    decode_l(Fst_Byte, Rest, RL + Len * Multiplier, Multiplier * 128);
%% Calculate Value of the remaining length : 
%% Return if the value of the first bit is 0.
decode_l(Fst_Byte, <<0:1, Len:7, Rest/binary>>, RL, Multiplier)   
    when ((RL + Len * Multiplier) =:= size(Rest)) ->
    {
      ok, 
      {first_byte, Fst_Byte},
      {remaining_length, RL + Len * Multiplier}, 
      {remaining_binary, Rest}      
    };
%% Rest of the message are having invalid lenght.
decode_l(_, _, _, _) -> 
    {error, invalid_remaining_length}.

%% Encode Length
encode_l(L) 
  when (L > ?MAX_LENGTH) ->
    {error, invalid_length};
encode_l(L) ->
    encode_l(<<>>, {L div 128, L rem 128}).

encode_l(Bin, {0, RBits}) ->
    list_to_binary([Bin, <<0:1, RBits:7>>]);
encode_l(Bin, {FBit, RBits}) ->
    encode_l(list_to_binary([Bin, <<1:1, RBits:7>>]), {FBit div 128, FBit rem 128}).


%% Identify message type.
route(Bin = <<1:4, _/binary>>) -> {ok, connect, Bin};
route(Bin = <<2:4, _/binary>>) -> {ok, connack, Bin};
route(Bin = <<3:4, _/binary>>) -> {ok, publish, Bin};
route(Bin = <<4:4, _/binary>>) -> {ok, puback, Bin};
route(Bin = <<5:4, _/binary>>) -> {ok, pubrec, Bin};
route(Bin = <<6:4, _/binary>>) -> {ok, pubrel, Bin};
route(Bin = <<7:4, _/binary>>) -> {ok, pubcomp, Bin};
route(Bin = <<8:4, _/binary>>) -> {ok, subscribe, Bin};
route(Bin = <<9:4, _/binary>>) -> {ok, suback, Bin};
route(Bin = <<10:4, _/binary>>) -> {ok, unsubscribe, Bin};
route(Bin = <<11:4, _/binary>>) -> {ok, unsuback, Bin};
route(Bin = <<12:4, _/binary>>) -> {ok, pingreq, Bin};
route(Bin = <<13:4, _/binary>>) -> {ok, pingresp, Bin};
route(Bin = <<14:4, _/binary>>) -> {ok, disconnect, Bin}.

%% EUnit Tests...
route_connect_test() ->
    ?assert({ok, connect, <<1:4, 23400>>} =:= route(<<1:4, 23400>>)).
route_conack_test() ->
    ?assert({ok, connack, <<2:4, 23400>>} =:= route(<<2:4, 23400>>)).
route_publish_test() ->
    ?assert({ok, publish, <<3:4, 23400>>} =:= route(<<3:4, 23400>>)).
route_puback_test() ->
    ?assert({ok, puback, <<4:4, 23400>>} =:= route(<<4:4, 23400>>)).
route_pubrec_test() ->
    ?assert({ok, pubrec, <<5:4, 23400>>} =:= route(<<5:4, 23400>>)).
route_pubrel_test() ->
    ?assert({ok, pubrel, <<6:4, 23400>>} =:= route(<<6:4, 23400>>)).
route_pubcomp_test() ->
    ?assert({ok, pubcomp, <<7:4, 23400>>} =:= route(<<7:4, 23400>>)).
route_subscribe_test() ->
    ?assert({ok, subscribe, <<8:4, 23400>>} =:= route(<<8:4, 23400>>)).
route_suback_test() ->
    ?assert({ok, suback, <<9:4, 23400>>} =:= route(<<9:4, 23400>>)).
route_unsubscribe_test() ->
    ?assert({ok, unsubscribe, <<10:4, 23400>>} =:= route(<<10:4, 23400>>)).
route_unsuback_test() ->
    ?assert({ok, unsuback, <<11:4, 23400>>} =:= route(<<11:4, 23400>>)).
route_pingreq_test() ->
    ?assert({ok, pingreq, <<12:4, 23400>>} =:= route(<<12:4, 23400>>)).
route_pingresp_test() ->
    ?assert({ok, pingresp, <<13:4, 23400>>} =:= route(<<13:4, 23400>>)).
route_disconnect_test() ->
    ?assert({ok, disconnect, <<14:4, 23400>>} =:= route(<<14:4, 23400>>)).
