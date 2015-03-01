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
%%%
%%% @end
%%% Created : 11 Sep 2013 by  KuldeepSinh Chauhan
%%%-------------------------------------------------------------------
-module(yams_lib).

-define(MAX_LENGTH, 268435455).
%% APIs
-export([
	 encode_l/1, %% encode length.
	 extract_str/1 %% retrieve string length and string from binary
	]).

%%=====================================
%% API
%%=====================================
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

%% Extract string from the packet
extract_str(Pkt) ->
    validate_string(get_string_fields(Pkt)).


%%=====================================
%% Internal
%%=====================================
% retrieve string length, string value and rest-bin from the Binary.
get_string_fields(<<>>) ->
    {error, empty_binary};
get_string_fields(<<Length:16, Rest/binary>>) ->
    get_string_fields(validate_bin_size(Length, Rest), Length, Rest);
get_string_fields(_) ->
    {error, invalid_length_field}.

get_string_fields(true, Length, Rest) ->
    {Value, RestBin} = split_binary(Rest, Length),
    {Length, Value, RestBin};
get_string_fields(false, _Length, _Rest) ->
    {error, length_mismatch}.

% Validate size of binary with respect to the associated length.
% If size of binary is >= the value of the length field, its valid.
validate_bin_size(Length, Rest) ->
    size(Rest) >= Length.  

%% Validate if string contains UTF-8 null character.
%% Rest of the UTF-8 validations are not requried,
%% because Erlang 17 comes with UTF-8 - RFC 3629 support out of the box.
validate_string({error, Reason}) ->
    {error, Reason};
validate_string({true, _Length, _Value, _RestBin}) ->
    {error, string_contains_null_char};
validate_string({false, Length, Value, RestBin}) ->
    {ok, Length, Value, RestBin};
validate_string({Length, Value, RestBin}) ->
    NullChar = $\0,
    %% Here, assumption is "Value" passed to the function is a string (list of chars) - e.g. "abc" is valid, 
    %% "Value" should not be list of string - ["abc"] is invalid.
    validate_string({lists:member(NullChar, binary:bin_to_list(Value)), Length, Value, RestBin}).
