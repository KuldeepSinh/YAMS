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

-module(router_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for router:validate_type
validate_type_less_than_1_test() ->
    ?assert({error, invalid_msg_type} =:= router:validate_type(<<0:4, 23400>>)).
validate_type_more_than_14_test() ->
    ?assert({error, invalid_msg_type} =:=  router:validate_type(<<15:4, 23400>>)).
validate_type_equal_1_test() ->
    ?assert({ok, valid_msg_type} =:=  router:validate_type(<<1:4, 23400>>)).
validate_type_equal_14_test() ->
    ?assert({ok, valid_msg_type} =:=  router:validate_type(<<14:4, 23400>>)).
validate_type_equal_7_test() ->
    ?assert({ok, valid_msg_type} =:=  router:validate_type(<<7:4, 23400>>)).

%% Tests for router:decode_l
decode_length_0_test() ->
    ?assert({ok, 
	     {remaining_length, 0}, 
	     {remaining_binary, <<>>}} =:= router:decode_l(<<0>>)).
decode_length_65_test() ->
    ?assert({ok, 
	     {remaining_length, 65}, 
	     {remaining_binary, <<1:520>>}} =:= router:decode_l(<<65, 1:520>>)).
decode_length_127_test() ->
    ?assert({ok, 
	     {remaining_length, 127}, 
	     {remaining_binary, <<1:1016>>}} =:= router:decode_l(<<127, 1:1016>>)).
decode_length_128_test() ->
    ?assert({ok, 
	     {remaining_length, 128}, 
	     {remaining_binary, <<1:1024>>}} =:= router:decode_l(<<128, 1, 1:1024>>)).
decode_length_8192_test() ->
    ?assert({ok, 
	     {remaining_length, 8192}, 
	     {remaining_binary, <<1:65536>>}} =:= router:decode_l(<<128, 64, 1:65536>>)).
decode_length_16383_test() ->
    ?assert({ok, 
	     {remaining_length, 16383}, 
	     {remaining_binary, <<1:131064>>}} =:= router:decode_l(<<255, 127, 1:131064>>)).
decode_length_1048575_test() ->
    ?assert({ok, 
	     {remaining_length, 1048575}, 
	     {remaining_binary, <<1:8388600>>}} =:= router:decode_l(<<255, 255, 63, 1:8388600>>)).
decode_length_2097151_test() ->
    ?assert({ok, 
	     {remaining_length, 2097151}, 
	     {remaining_binary, <<1:16777208>>}} =:= router:decode_l(<<255, 255, 127, 1:16777208>>)).
decode_length_2097152_test() ->
    ?assert({ok, 
	     {remaining_length, 2097152}, 
	     {remaining_binary, <<1:16777216>>}} =:= router:decode_l(<<128, 128, 128, 1, 1:16777216>>)).
decode_length_134217727_test() ->
    ?assert({ok, 
	     {remaining_length, 134217727}, 
	     {remaining_binary, <<1:1073741816>>}} =:= router:decode_l(<<255, 255, 255, 63, 1:1073741816>>)).
decode_length_134217728_test() ->
    ?assert({ok, 
	     {remaining_length, 134217728}, 
	     {remaining_binary, <<1:1073741824>>}} =:= router:decode_l(<<128, 128, 128, 64, 1:1073741824>>)).
decode_length_268435455_test() ->
    ?assert({ok, 
	     {remaining_length, 268435455}, 
	     {remaining_binary, <<1:2147483640>>}} =:= router:decode_l(<<255, 255, 255, 127, 1:2147483640>>)).
decode_length_268435456_test() ->
    ?assert({error, invalid_remaining_length} =:= router:decode_l(<<255, 255, 255, 127, 1:2147483648>>)).

%% Test for router:encode_l
encode_length_0_test() ->
    ?assert(<<0>> =:= router:encode_l(0)).
encode_length_65_test() ->
    ?assert(<<65>> =:= router:encode_l(65)).
encode_length_127_test() ->
    ?assert(<<127>> =:= router:encode_l(127)).
encode_length_128_test() ->
    ?assert(<<128,1>> =:= router:encode_l(128)).
encode_length_8192_test() ->
    ?assert(<<128, 64>> =:= router:encode_l(8192)).
encode_length_16383_test() ->
    ?assert(<<255, 127>> =:= router:encode_l(16383)).
encode_length_16384_test() ->
    ?assert(<<128, 128, 1>> =:= router:encode_l(16384)).
encode_length_1048575_test() ->
    ?assert(<<255, 255, 63>> =:= router:encode_l(1048575)).
encode_length_1048576_test() ->
    ?assert(<<128, 128, 64>> =:= router:encode_l(1048576)).
encode_length_2097151_test() ->
    ?assert(<<255, 255, 127>> =:= router:encode_l(2097151)).
encode_length_2097152_test() ->
    ?assert(<<128, 128, 128, 1>> =:= router:encode_l(2097152)).
encode_length_134217727_test() ->
    ?assert(<<255, 255, 255, 63>> =:= router:encode_l(134217727)).
encode_length_134217728_test() ->
    ?assert(<<128, 128, 128, 64>> =:= router:encode_l(134217728)).
encode_length_268435455_test() ->
    ?assert(<<255, 255, 255, 127>> =:= router:encode_l(268435455)).
encode_length_268435456_test() ->
    ?assert({error, invalid_length} =:= router:encode_l(268435456)).



%% Tests for router:route
route_connect_test() ->
    ?assert({ok, connect, <<1:4, 23400>>} =:=  router:route(<<1:4, 23400>>)).
route_conack_test() ->
    ?assert({ok, connack, <<2:4, 23400>>} =:=  router:route(<<2:4, 23400>>)).
route_publish_test() ->
    ?assert({ok, publish, <<3:4, 23400>>} =:=  router:route(<<3:4, 23400>>)).
route_puback_test() ->
    ?assert({ok, puback, <<4:4, 23400>>} =:=  router:route(<<4:4, 23400>>)).
route_pubrec_test() ->
    ?assert({ok, pubrec, <<5:4, 23400>>} =:=  router:route(<<5:4, 23400>>)).
route_pubrel_test() ->
    ?assert({ok, pubrel, <<6:4, 23400>>} =:=  router:route(<<6:4, 23400>>)).
route_pubcomp_test() ->
    ?assert({ok, pubcomp, <<7:4, 23400>>} =:=  router:route(<<7:4, 23400>>)).
route_subscribe_test() ->
    ?assert({ok, subscribe, <<8:4, 23400>>} =:=  router:route(<<8:4, 23400>>)).
route_suback_test() ->
    ?assert({ok, suback, <<9:4, 23400>>} =:=  router:route(<<9:4, 23400>>)).
route_unsubscribe_test() ->
    ?assert({ok, unsubscribe, <<10:4, 23400>>} =:=  router:route(<<10:4, 23400>>)).
route_unsuback_test() ->
    ?assert({ok, unsuback, <<11:4, 23400>>} =:=  router:route(<<11:4, 23400>>)).
route_pingreq_test() ->
    ?assert({ok, pingreq, <<12:4, 23400>>} =:=  router:route(<<12:4, 23400>>)).
route_pingresp_test() ->
    ?assert({ok, pingresp, <<13:4, 23400>>} =:=  router:route(<<13:4, 23400>>)).
route_disconnect_test() ->
    ?assert({ok, disconnect, <<14:4, 23400>>} =:=  router:route(<<14:4, 23400>>)).
