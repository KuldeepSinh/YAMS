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
-export([chk_type/1, chk_l/1, route/1]).
-include_lib("eunit/include/eunit.hrl").

chk_type(Bin = <<MT:4, _/binary>>) 
  when ((MT <1) or (MT > 14)) ->
    {error, invalid_msg_type, Bin};
chk_type(Bin) ->
    {ok, valid_msg_type, Bin}.


chk_l(_) ->
    ok.


route(Bin = <<MT:4, _/binary>>) ->
    case MT of 
	1 ->
	    {ok, connect, Bin};
	2 ->
	    {ok, conack, Bin};
	3 ->
	    {ok, publish, Bin};
	4 ->
	    {ok, puback, Bin};
	5 ->
	    {ok, pubrec, Bin};
	6 ->
	    {ok, pubrel, Bin};
	7 ->
	    {ok, pubcomp, Bin};
	8 ->
	    {ok, subscribe, Bin};
	9 ->
	    {ok, suback, Bin};
	10 ->
	    {ok, unsubscribe, Bin};
	11 ->
	    {ok, unsuback, Bin};
	12 ->
	    {ok, pingreq, Bin};
	13 ->
	    {ok, pingresp, Bin};
	14 ->
	    {ok, disconnect, Bin}
	end.



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



route_connect_test() ->
    ?assert({ok, connect, <<1:4, 23400>>} =:= route(<<1:4, 23400>>)).
route_conack_test() ->
    ?assert({ok, conack, <<2:4, 23400>>} =:= route(<<2:4, 23400>>)).
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

