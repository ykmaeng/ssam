%% ------------------------------------------------------------------------
%% Copyright (c) 2014, Kook Maeng <kook.maeng@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%% ------------------------------------------------------------------------

-define(SERVICE, <<"ssam_message">>).

-define(MIN_SEC, 60).
-define(HOUR_SEC, ?MIN_SEC*60).
-define(DAY_SEC, ?HOUR_SEC*24).
-define(WEEK_SEC, ?DAY_SEC*7).


%% Topic Resource
-define(TOPIC_SERVER_TIMEOUT, ?MIN_SEC*1000).
%-define(TOPIC_MAX_TRAN_CNT, 1000). %% @todo can be changed by user
-define(TOPIC_MESSAGE_TTL, ?MIN_SEC).
-define(TOPIC_CLIENT_TTL, ?MIN_SEC).
-define(TOPIC_FALLBACK_NODE_CNT, 1).
-define(TOPIC_HTTPLOOP_TIMEOUT, ?TOPIC_CLIENT_TTL*1000).
-define(TOPIC_WEBSOCKET_TIMEOUT, ?MIN_SEC*3*1000).

-define(TOPIC_MAX_BYTES, 1024*100).
-define(TOPIC_MAX_COUNT, 1000).
-define(TOPIC_CURSOR_TTL, ?WEEK_SEC).
-define(TOPIC_MSG_DELIMITER, <<$\r$\r>>).
-define(TOPIC_REST_DELIMITER, <<$:>>).
-define(TOPIC_MQTT_DELIMITER, <<$/>>).
-define(TOPIC_WEBHOOK_TIMEOUT, 3*1000).
-define(TOPIC_FALLBACK_CNT, 2).

-define(BUCKET_TOPIC_SERVER, <<?SERVICE/binary, 101:16>>).
-define(BUCKET_TOPIC_CURSORS, <<?SERVICE/binary, 102:16>>).
-define(BUCKET_TOPIC_SUBSCRIBERS, <<?SERVICE/binary, 103:16>>).

%% Topic node's roles
-define(primary, primary).
-define(fallback, fallback).

%% Topic commands
-define(subscribe, subscribe).
-define(unsubscribe, unsubscribe).
-define(publish, publish).

%% Topic transports
-define(rest, rest).
-define(eventsource, eventsource).
-define(websocket, websocket).
-define(mqtt, mqtt).
-define(ws_mqtt, ws_mqtt).

%% Topic subscriber's types
-define(pid, pid).
-define(webhook, webhook).
-define(email, email).
-define(sms, sms).
-define(mms, mms).

%% Events
-define(msg_takeout, msg_takeout).
-define(msg_arrived, msg_arrived).
-define(topic_empty, topic_empty).

%% Errors
-define(no_pid, no_pid).
-define(invalid_topic, invalid_topic).
-define(invalid_client, invalid_client).
-define(invalid_url, invalid_url).
-define(invalid_response, invalid_response).
-define(no_topic_messages, no_topic_messages).
-define(no_topic_receivers, no_topic_receivers).
-define(no_topic_clients, no_topic_clients).
-define(no_topic_subscribers, no_topic_subscribers).
-define(no_valid_subscribers, no_valid_subscribers).
-define(invalid_subscriber_type, invalid_subscriber_type).
-define(topic_already_exists, topic_already_exists).
-define(topic_key_not_matched, topic_key_not_matched).
-define(topic_server_not_available, topic_server_not_available).
-define(cursor_already_exists, cursor_already_exists).
-define(pid_not_found, pid_not_found).
-define(topic_not_found, topic_not_found).
-define(nodes_not_available, nodes_not_available).
-define(primary_node_not_available, primary_node_not_available).
-define(fallback_nodes_not_available, fallback_nodes_not_available).
-define(fallback_nodes_lack, fallback_nodes_lack).
-define(fallback_nodes_updated, fallback_nodes_updated).

-define(no_receivers, no_receivers). 

%% Errors for /message//sms
-define(provider_handler_not_found, provider_handler_not_found).

%% Riak Buckets
-define(BUCKET_MESSAGE_LOG, <<"ssam_message:log">>).
-define(BUCKET_MESSAGE_SMS, <<"ssam_message:sms">>).
-define(BUCKET_MESSAGE_LMS, <<"ssam_message:lms">>).

%% SMS/LMS/MMS Result Codes
-define(MSG_OK, 200).
-define(MSG_ERROR, 400). 
-define(MSG_INVALID_FORMAT, 401). 
-define(MSG_INVALID_NUMBER, 402). 
-define(MSG_INVALID_MEDIA, 403). 
-define(MSG_LENGTH_OVER, 404). 
-define(MSG_MEMORY_FULL, 405). 
-define(MSG_EXPIRED, 406). 
-define(MSG_POWER_OFF, 407). 
-define(MSG_SPAMMED, 408).
-define(MSG_NOT_SUPPORTED, 409).
-define(MSG_SERVER_ERROR, 500).



%% Records

-record(topic, {
		key :: ssam_message_topic:key(),
		parts :: list(binary()),
		path :: ssam_message_topic:path(),
		fallback_cnt = 0 :: 0 | pos_integer(),
		props = [] :: [] | ssam:props()
	}).

-record(topic_request, {
		id :: ssam:request_id(),
		account_sid :: ssam:account_sid(),
		command :: ssam_message_topic:command(),
		transport :: ssam_message_topic:transport(),
		action :: ssam_message_topic:action(),
		topic :: #topic{},
		body :: ssam_message_topic:request_props() | record(),
		timestamp :: ssam:timestamp()
	}).

-record(topic_message, {
		topic_key :: ssam_message_topic:key(),
		seq :: pos_integer(),
		msg_id :: binary(),
		msg_body :: term(),
		timestamp :: ssam:timestamp(),
		props = [] :: [] | ssam:props()
	}).

-record(topic_cursor, {
		key :: ssam_message_topic_cursor:key(),
		last_seq :: 0 | pos_integer(),
		last_use :: ssam:timestamp(),
		props = [] :: [] | ssam:props()
	}).


-record(topic_subpid, {
	    type :: ssam_message_topic:subscribe_type(),
		stream :: yes | no,
		from :: pid(),
		cursor :: binary(),
		last_seq  = 0 :: 0 | pos_integer(),
		count = 0 :: 0 | pos_integer(),
		extras = [] :: [] | ssam:props()
	}).

-record(topic_publish, {
		data :: binary(),
		from :: pid(),
		extras = [] :: [] | ssam:props()
	}).
