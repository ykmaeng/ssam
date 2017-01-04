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

-module(ssam_message_topic_resource).

%-compile(export_all).
-export([get/2, post/2, put/2, delete/2]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

get(#request{
	   uri = [<<"topics">>, TopicId, <<"updates">>],
	   account_sid = Sid, params = Params
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:get_topics_updates(
		   Request#request.id, Sid, TopicId,
		   [{from, Request#request.from},
			{cursor, ssam_objects:value(<<"cursor">>, Params, <<>>)},
			{last_seq, ssam_objects:value(<<"last_seq">>, Params, 0)},
			{count, ssam_objects:value(<<"count">>, Params, 0)}]
	) of
		{ok, TopicKey} ->
			Props = [{topic_key, TopicKey} | Request#request.body],
			Request1 = Request#request{body = Props},
			{ok, Request1};
		{?error, Reason} ->
			lager:error("~p, TopicId: ~p", [Reason, TopicId]),
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Reason, Request)}
	end;

get(#request{
	   uri = [<<"topics">>],
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:get_topics(AccountSid) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

get(#request{
	   uri = Uri,
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_resource:get(AccountSid, ?SERVICE, Uri) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

post(#request{
		uri = [<<"topics">>],
		account_sid = AccountSid, body = Body
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:post_topic(AccountSid, Body) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end;

post(#request{
		uri = [<<"topics">>, TopicId, <<"updates">>]
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:post_topics_update(Request#request.account_sid, TopicId,
										 Request#request.id, Request#request.body) of
		ok ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Reason} ->
			lager:error("~p, TopicId: ~p", [Reason, TopicId]),
			{ok, ssam_rest:response(?STATUS_CREATED, Request)}
	end;

post(#request{
		uri = [<<"topics">>, TopicId, <<"subscribers">>]
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:post_topics_subscriber(Request#request.account_sid, TopicId,
									  Request#request.body) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			lager:warning("~p, TopicId: ~p", [Error, TopicId]),
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end.


put(#request{
		uri = [<<"topics">>, TopicId]
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	#request{account_sid = AccountSid, body = Params} = Request,
	case ssam_message:put_topic(AccountSid, TopicId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

put(#request{
		uri = [<<"topics">>, TopicId, <<"subscribers">>, SubId],
		account_sid = AccountSid, body = Params
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:put_topics_subscriber(AccountSid, TopicId, SubId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

put(#request{
		uri = Uri,
		account_sid = AccountSid, body = Params
	   } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_resource:put(AccountSid, ?SERVICE, Uri, {Params}) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

delete(#request{
	   uri = [<<"topics">>],
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:delete_topics(AccountSid) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

delete(#request{
	   uri = [<<"topics">>, TopicId],
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:delete_topic(AccountSid, TopicId) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

delete(#request{
	   uri = [<<"topics">>, TopicId, <<"subscribers">>],
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:delete_topics_subscribers(AccountSid, TopicId) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

delete(#request{
	   uri = [<<"topics">>, TopicId, <<"subscribers">>, SubId],
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_message:delete_topics_subscriber(AccountSid, TopicId, SubId) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

delete(#request{
	   uri = Uri,
	   account_sid = AccountSid
	  } = Request, Sender) ->
	lager:info("RequestId: ~p, Path: ~p, Sender: ~p",
			   [Request#request.id, Request#request.path, Sender]),
	case ssam_resource:delete(AccountSid, ?SERVICE, Uri) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.



%% Private functions




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
