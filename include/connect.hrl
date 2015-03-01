%% %% Copyright 2013, 2014, 2015 KuldeepSinh Chauhan
%% %%
%% %% Licensed under the Apache License, Version 2.0 (the "License");
%% %% you may not use this file except in compliance with the License.
%% %% You may obtain a copy of the License at
%% %%
%% %% http://www.apache.org/licenses/LICENSE-2.0
%% %%
%% %% Unless required by applicable law or agreed to in writing, software
%% %% distributed under the License is distributed on an "AS IS" BASIS,
%% %% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% %% See the License for the specific language governing permissions and
%% %% limitations under the License.

%% %%%-------------------------------------------------------------------
%% %%% @author  KuldeepSinh Chauhan
%% %%% @copyright (C) 2013, 2014, 2015 
%% %%% @doc
%% %%%    record information retrieved from the connect control packet.
%% %%% @end
%% %%% Created : 21 Feb 2015 by  KuldeepSinh Chauhan
%% %%%-------------------------------------------------------------------
-record(conn_flags, 
	{
	  user, %% user name
	  password, %% password
	  will_retain, 
	  will_qos, 
	  will, 
	  clean_session,
	  reserved
	}
       ).
%% conn_var_head recoud should contain value of all the connection flags and the value of the keep alive time
-record(conn_var_head, 
	{
	  conn_flags, %% connect flags defineds as above.
	  kat %% keep alive time
	}
       ).
%% conn_pkt record should contian value of variable header and payload.
-record(conn_pkt, 
	{
	  conn_var_head, 
	  payload
	}
       ).
%% client is a composit field containing client-id, user name and password.
-record(client, 
	{
	  client_id, %% value of client ID (retrieved from the connect payload)
	  username, %% value of client's username (retrieved from the connect payload)
	  password
	}
       ).
%% will record should be used for the will management.
-record(conn_will,
	{
	  client, %% full id of client as defined above
	  will, %% value stored in the will flag
	  will_qos, %% value stored in the will quality of service flag
	  will_retain, %% value stored in the will retain flag
	  will_topic, %% value of the will topic (retrieved from the connect payload)
	  will_msg %% value of the will message (retrieved from the connect payload)
	}
       ).
%% session record should be used for the session management.
-record(session,
	{
	  client, %% full id of client as defined above
	  session_id, %% session id of the client
	  clean_session %% value of the clean session flag
	}
       ).
