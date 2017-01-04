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

-module(ssam_message).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").


%% Callback
-export([
		 configs/0,
		 routes/0
		]).

%% Public
-export([
		 get_topics/1,
		 get_topic/2,
		 get_topics_updates/4
		]).


%% Callback functions

configs() ->
	File = code:priv_dir(ssam_message) ++ "/conf/ssam_message.conf",
	case file:consult(File) of
		{ok, Props} -> Props;
		{error, _} -> ?undefined 
	end.

routes() ->
	proplists:get_value(routes, configs(), ?undefined).

%% Public functions


get_topics(AccountSid) ->
	Uri = [{<<"topics">>, []}],
	case ssam_resource:get(AccountSid, ?SERVICE, Uri) of
		{ok, Doc} ->
			{ok, Doc};
		{?error, {?invalid_collection_name, _, _}} ->
			{ok, []};
		{?error, Error} ->
			{?error, Error}
	end.

get_topic(AccountSid, TopicId) ->
	Uri = [{<<"topics">>, TopicId}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).


get_topics_updates(ReqId, AccountSid, TopicId, Props) ->
	Topic = topic(AccountSid, TopicId),
	TopicReq =
		#topic_request{
		   id = ReqId,
		   account_sid = AccountSid,
		   topic = Topic,
		   command = ?subscribe,
		   transport = ?rest,
		   body = Props
	},
	ok = ssam_message_topic:route(TopicReq),
	{ok, Topic#topic.key}.

	


topic(AccountSid, TopicId) ->
	case ssam_message_topic:topic(AccountSid, TopicId) of
		?undefined ->
			ssam_message_topic:new(AccountSid, TopicId);
		Topic ->
			Topic
	end.

