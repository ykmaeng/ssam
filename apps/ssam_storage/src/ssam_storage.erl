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

-module(ssam_storage).

-export([configs/0,
		 routes/0,
		 post_buckets_key/5,
		 get_buckets_key/3,
		 put_buckets_key/5]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_storage.hrl").


%% Callback functions

configs() ->
	File = code:priv_dir(ssam_storage) ++ "/conf/ssam_storage.conf",
	case file:consult(File) of
		{ok, Props} -> Props;
		{error, _} -> []
	end.

routes() ->
	case proplists:get_value(routes, configs()) of
		?undefined -> [];
		Routes -> Routes
	end.


%% Public functions

post_buckets_key(Sid, Bucket, KeyId, Type, Body) ->
	Key = key([Sid, Bucket, KeyId]),
	Doc = {[
			{<<"id">>, KeyId},
			{<<"type">>, Type},
			{<<"body">>, Body},
			{<<"created">>, ssam_util:now_sec()}
		   ]},
	case riak_put(Key, Doc) of
		ok -> {ok, Doc};
		{?error, Reason} -> {?error, Reason}
	end.

get_buckets_key(Sid, Bucket, KeyId) ->
	Key = key([Sid, Bucket, KeyId]),
	case riak_get(Key) of
		{ok, DocBin} when is_binary(DocBin) ->
			Doc = binary_to_term(DocBin),
			Type = ssam_objects:value(<<"type">>, Doc, <<>>),
			Body = ssam_objects:value(<<"body">>, Doc, <<>>),
			{ok, {Type, Body}};
		{?error, Reason} ->
			{?error, Reason}
	end.

put_buckets_key(Sid, Bucket, KeyId, Type, Body) ->
	Key = key([Sid, Bucket, KeyId]),
	Doc = {[
			{<<"type">>, Type},
			{<<"body">>, Body},
			{<<"created">>, ssam_util:now_sec()}
		   ]},
	case riak_put(Key, Doc) of
		ok -> {ok, Doc};
		{?error, Reason} -> {?error, Reason}
	end.


%% Private functions

key(List) -> ssam_util:combined(List).

riak_put(Key, Val) when is_binary(Val) ->
	ssam_riakc:put(?BUCKET_STORAGE_BUCKET, Key, Val);
riak_put(Key, Val) ->
	ssam_riakc:put(?BUCKET_STORAGE_BUCKET, Key, term_to_binary(Val)).

riak_get(Key) ->
	ssam_riakc:get(?BUCKET_STORAGE_BUCKET, Key).

