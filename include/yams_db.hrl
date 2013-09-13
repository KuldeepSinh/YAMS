%% cid_to_apid table will map Client ID with the associated
%% Acceptor PId.

%% Note: cid is chosen as the first field in this record, 
%% so that it will be the primary key, in the mnesia table.
-record(cid_to_apid, 
	{
	  cid, %% Client ID
	  apid %% Acceptor PID
	}).


%% subcription table will map Client ID with the topics 
%% subscribed/unsubscribed by the client.
-record(subscription, 
	{
	  cid, %% Client ID
	  topic, %% Topic
	  qos, %% QoS
	  msgID %% Message ID
	}).

%% publication table will store MsgID, Topic and Message
-record(publication, 
	{
	  %%yams_msg_ID, %% Unique messag ID assigned by YAMS
	  cid, %% Client ID
	  msgID, %% Message ID
	  topic, %% Topic of the message
	  dup, %% Duplicate flag
	  qos, %% Quality of Service
	  retain, %% Retain Flag
	  msg, %% Message Published (does not contain first byte and remaining lenght field)
	  is_retained %% Boolean flag (identifies if the message is the retained one (by YAMS)
	}).

