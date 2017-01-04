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

-module(ssam_storage_bucket_resource).

-export([get/2,
		 put/2,
		 post/2]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_storage.hrl").

%% @todo Reduce the logic of state functions

%% Publics

post(#request{
	   uri = [<<"buckets">>, Bucket, <<"keys">>],
	   id = ReqId, account_sid = Sid
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	%{Type, SubType, _Param} = Request#request.content_type,
	%ContentType = <<Type/bits, $/, SubType/bits>>,
	ContentType = Request#request.content_type, 
	Body = Request#request.body,
	case ssam_storage:post_buckets_key(Sid, Bucket, ReqId, ContentType, Body) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Reason} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Reason, Request)}
	end.

get(#request{
	   uri = [<<"buckets">>, Bucket, <<"keys">>, Key],
	   account_sid = Sid
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_storage:get_buckets_key(Sid, Bucket, Key) of
		{ok, {Type, Body}} ->
			{ok, ssam_rest:response_type(Type, Body, Request)};
		{?error, Reason} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Reason, Request)}
	end.

put(#request{
	   uri = [<<"buckets">>, Bucket, <<"keys">>, Key],
	   account_sid = Sid
	  } = Request, Sender) ->
	lager:info("~p:put -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	Type = Request#request.content_type,
	Body = Request#request.body,
	case ssam_storage:put_buckets_key(Sid, Bucket, Key, Type, Body) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

%% Privates

