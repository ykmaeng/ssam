-define(SERVICE, <<"ssam_telephony">>).


%% Buckets for Riak
-define(TELEPHONY_BUCKET_ACCOUNTS, <<"telephony.directory.accounts">>).
-define(TELEPHONY_BUCKET_DOMAINS, <<"telephony.directory.domains">>).
-define(TELEPHONY_BUCKET_CONTEXTS, <<"telephony.dialplan.contexts">>).

%% Errors
-define(telephony_node_unloaded, telephony_node_unloaded).


%% Events
-define(call_incoming, call_incoming).
-define(call_placing, call_placing).
-define(call_connected, call_connected).
-define(call_disconnected, call_disconnected).

-record(telephony_channel, {
	node_fs :: undefined | atom(),
	uuid :: undefined | binary(),
	data :: undefined | list(tuple())
}).
