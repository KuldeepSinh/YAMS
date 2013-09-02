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

length(Topic) ->
    string:len(Topic).

null_position(Topic) ->
    string:chr(Topic, $\0).

contains_null_char(0) ->
    false;
contains_null_char(_) ->
    true.

validate_length(0) ->
    {error, invalid_length};
validate_length(65535) ->
    {error, invalid_length};
validate_length(_) ->
    {ok, valide_length}.

check_repeated_separators(Topic) ->
    re:run(Topic,"//").

validate_separator({match, _}) ->
    {error, invalid_level_separator};
validate_separator(_) ->
    {ok, valid}.

trim_whitespace(Topic) ->
    %% First remove left side spaces.
    LS= re:replace(Topic, "^[ \t]*", "", [{return, list}]),
    %% Then remove right side spaces.
    re:replace(LS, "[ \t]*$", "", [{return, list}]).

