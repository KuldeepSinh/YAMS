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

-module(topic).
-compile(export_all).

%% Find the position of the NULL character in the topic, if any.
null_position(Topic) ->
    string:chr(Topic, $\0).
%% If NULL character is not available, false will be return.
contains_null_char(0) ->
    false;
%% If NULL character is available, true will be return.
contains_null_char(_) ->
    true.

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

%% Check if level separator is repeated consecutively.
check_consecutive_separators(Topic) ->
    re:run(Topic,"//").

%% If level separator is repeated consecutively, invalid.
validate_separator({match, _}) ->
    {error, invalid_level_separator};
validate_separator(_) ->
    {ok, valid}.

%% Trim leading and trailing white-spaces from the string.
trim_whitespace(Topic) ->
    %% First remove left side spaces.
    LS= re:replace(Topic, "^[ \t]*", "", [{return, list}]),
    %% Then remove right side spaces.
    re:replace(LS, "[ \t]*$", "", [{return, list}]).

%% Check if multi-level wildcard is available.
check_multi_level_wildcard(Topic) ->
    re:run(Topic,"#", [global]).
%% if not avalable, then multi-level wildcard test is pass.
validate_multi_level_wildcard(nomatch, _Topic) -> 
    {ok, valid};
%%if available...
validate_multi_level_wildcard({match, List}, Topic) -> 
    %% check total number of occurances of the multi-level wildcard...
    case (erlang:length(List)) of
	%% if exactly one occurance is there for the multi-level wildcard...
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
	%% if more than one occurance is there of the multi-level wild card, topic is invalid.
	_ ->
	    {error, invalid}
    end.

%% if the topic ends with "/#", the topic is valid.
validate_multi_level_wildcard_on_end({match, _}) ->
    {ok, valid};
%% if the topic does not edn with "/#", the topic is invalid.
validate_multi_level_wildcard_on_end(_) ->
    {error, invalid}.

    
