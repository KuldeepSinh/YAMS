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

%% ==============================================
%% MOST OF THIS MODULE WILL NOT BE USE,        %%
%% INSTEAD TOPIC VALIDATION/PARSING WILL BE    %%
%% HANDLED WITH THE HELP OF gen_fsm.           %% 
%% ==============================================


-module(topic).
-compile(export_all).

%%==================================
%% Find the position of the NULL character in the topic, if any.
null_position(Topic) ->
    string:chr(Topic, $\0).
%% If NULL character is not available, false will be return.
contains_null_char(0) ->
    false;
%% If NULL character is available, true will be return.
contains_null_char(_) ->
    true.
%%==================================
%% Find length of the topic
length(Topic) ->
    string:len(Topic).
%% Validate length (zero is invalid)
validate_length(0) ->
    {error, invalid_length};
%% Less than 65535 valid
validate_length(N) 
  when (N < 65535)->
    {ok, valide_length};
%% Validate length (greater than 65535 is invalid)
validate_length(_) ->
    {error, invalid_length}.
%%==================================
%% Check if an expression is present 
%% Intended use: we will check if // and ++ are present in the topic.
check_presence(Topic, Expression) ->
    re:run(Topic, Expression).    
%% If characters are repeated consecutively, topic is invalid.
%% Should be used with check_presence/2
validate_for_consecutive_chars({match, _}) ->
    {error, invalid};
validate_for_consecutive_chars(_) ->
    {ok, valid}.
%%==================================
%% Trim leading and trailing white-spaces from the string.
trim_whitespace(Topic) ->
    %% First remove left side spaces.
    LS = re:replace(Topic, "^[ \t]*", "", [{return, list}]),
    %% Then remove right side spaces.
    re:replace(LS, "[ \t]*$", "", [{return, list}]).
%%==================================
%% Check if multi-level wild-card is available.
check_multi_level_wildcard(Topic) ->
    re:run(Topic,"#", [global]).
%% if not available, then multi-level wild-card test is pass.
validate_multi_level_wildcard(nomatch, _Topic) -> 
    {ok, valid};
%%if available...
validate_multi_level_wildcard({match, List}, Topic) -> 
    %% check total number of occurrences of the multi-level wild-card...
    case (erlang:length(List)) of
	%% if exactly one occurrence is there for the multi-level wild-card...
	1 ->
	    %% check the length of the topic
	    case (erlang:length(Topic)) of
		%% if it is exactly one character long, its valid...
		1 ->
		    {ok, valid};
		%% if its more than one character is there in a topic...
		_ ->
		    %% check how it ends....
		    validate_multi_level_wildcard_on_end(re:run(Topic, "/#$"))			
	    end;
	%% if more than one occurrence is there of the multi-level wild card, topic is invalid.
	_ ->
	    {error, invalid}
    end.
%% if the topic ends with "/#", the topic is valid.
validate_multi_level_wildcard_on_end({match, _}) ->
    {ok, valid};
%% if the topic does not end with "/#", the topic is invalid.
validate_multi_level_wildcard_on_end(_) ->
    {error, invalid}.
%%==================================
%% Check if single level wild-card is available
check_single_level_wildcard(Topic) ->
    re:run(Topic,"+").
%% if not available, then multi-level wild-card test is pass.
validate_single_level_wildcard(nomatch, _Topic) -> 
    {ok, valid};
%%if available...
validate_single_level_wildcard({match, _}, Topic) ->
    case (erlang:length(Topic)) of
    1 -> 
        {ok, valid};
    _ ->
        %% remove all the occurrences of /+/ (slash plus slash).        
        T1 = remove_all_sps_occurrences(Topic),
        T2 = remove_all_leading_ps_occurrences(T1),
        T3 = remvoe_all_trailing_sp_occurrences(T2),
        Presence = check_presence(T3, "\\+"),
        validate_single_level_wildcard(Presence)
    end.    

remove_all_sps_occurrences(Topic) ->
    T1 = re:replace(Topic, "/\\+/", "/", [{return, list}]),
    case (Topic =:= T1) of 
	true ->
	    Topic;
	_  ->
	    remove_all_sps_occurrences(T1)
    end.

remove_all_leading_ps_occurrences(Topic) ->
    T1 = re:replace(Topic, "^\\+/", "", [{return, list}]),
    case (Topic =:= T1) of
        true ->
            Topic;
        _ ->
            remove_all_leading_ps_occurrences(T1)
    end.
remvoe_all_trailing_sp_occurrences(Topic) ->
    T1 = re:replace(Topic, "/\\+$", "", [{return, list}]),
    case (Topic =:= T1) of
        true ->
            T1;
        _ ->
            remvoe_all_trailing_sp_occurrences(T1)            
    end.
validate_single_level_wildcard({match, _}) ->
    {error, invalid};
validate_single_level_wildcard(_) ->
    {ok, valid}.
