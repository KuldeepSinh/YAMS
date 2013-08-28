%% cid_to_apid table will map Client ID with the associated
%% Acceptor PId.

%% Note: cid is chosen as the first field in this record, 
%% so that it will be the primary key, in the mnesia table.
-record(cid_to_apid, {cid, apid}).


%% subcription table will map Client ID with the topics 
%% client is subscribing to.
-record(subscription, {cid, topic}).
