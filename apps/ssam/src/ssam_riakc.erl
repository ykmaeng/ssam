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

-module(ssam_riakc).

-behaviour(gen_server).
%-behaviour(poolboy_worker).

-export([
		start_link/0,
		ping/0,
		put/3,
		get/2,
		delete/2,
		keys/1,
		list_buckets/0,
		set_bucket/2,
		key_filters/2,
		map/1,
		mapred/1,
		clear/1
	]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
		  conn :: undefined | pid()
		 }).

ping() ->
	gen_server:call(?MODULE, ping).

put(Bucket, Key, Value) ->
	gen_server:call(?MODULE, {put, Bucket, Key, Value}).

get(Bucket, Key) ->
	gen_server:call(?MODULE, {get, Bucket, Key}).

delete(Bucket, Key) ->
	gen_server:call(?MODULE, {delete, Bucket, Key}).

keys(Bucket) ->
	gen_server:call(?MODULE, {keys, Bucket}).

list_buckets() ->
	gen_server:call(?MODULE, list_buckets).

set_bucket(Bucket, BucketProps) ->
	gen_server:call(?MODULE, {set_bucket, Bucket, BucketProps}).

key_filters(Bucket, KeyFilters) ->
	gen_server:call(?MODULE, {key_filters, Bucket, KeyFilters}).

map(BucketKeyPairs) ->
	gen_server:call(?MODULE, {map, BucketKeyPairs}).

mapred(BucketKeyPairs) ->
	gen_server:call(?MODULE, {mapred, BucketKeyPairs}).

clear(Bucket) ->
	gen_server:call(?MODULE, {clear, Bucket}).


%% Callbacks
%%
start_link() ->
	{ok, Props} = application:get_env(ssam, riakc),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

init(Args) ->
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    {ok, Conn} = riakc_pb_socket:start_link(Host, Port),
    {ok, #state{conn=Conn}}.

handle_call(ping, _From, #state{conn=Conn}=State) ->
	{reply, riakc_pb_socket:ping(Conn), State};
handle_call({put, Bucket, Key, Value}, _From, #state{conn=Conn}=State) ->
	Object = riakc_obj:new(Bucket, Key, Value),
	case riakc_pb_socket:put(Conn, Object) of
		ok ->
			lager:debug("put ok, Bucket: ~p, Key: ~p", [Bucket, Key]),
			{reply, ok, State};
		{error, Reason} ->
			lager:error("put error, ~p", [Reason]),
			{reply, {error, Reason}, State}
	end;
handle_call({get, Bucket, Key}, _From, #state{conn=Conn}=State) ->
	Result = case riakc_pb_socket:get(Conn, Bucket, Key) of
		{ok, Object} -> {ok, riakc_obj:get_value(Object)};
		{error, notfound} -> {error, not_found};
		{error, Reason} ->
			lager:error("get, Reason: ~p, Bucket: ~p, Key: ~p", [Reason, Bucket, Key]),
			{error, Reason};
		Else -> {error, Else}
	end,
    {reply, Result, State};
handle_call({delete, Bucket, Key}, _From, #state{conn=Conn}=State) ->
	ok = riakc_pb_socket:delete(Conn, Bucket, Key),
	lager:debug("delete, Bucket: ~p, Key: ~p", [Bucket, Key]),
    {reply, ok, State};
handle_call({key_filters, Bucket, Filters}, _From, #state{conn=Conn}=State) ->
	Result = case riakc_pb_socket:mapred(Conn, {Bucket, Filters}, []) of
		{ok, []} -> {ok, []};
		{ok, [{_, Values}]} -> {ok, Values};
		Else -> Else
	end,
	lager:debug("key_filters, Bucket: ~p, Filters: ~p", [Bucket, Filters]),
	{reply, Result, State};
handle_call({map, BucketKeyPairs}, _From, #state{conn=Conn}=State) ->
	Result = case riakc_pb_socket:mapred(Conn, BucketKeyPairs, [
		{map, {modfun, riak_kv_mapreduce, map_object_value}, undefined, true}
	]) of
		{ok, [{0, Results}]} -> {ok, Results};
		{ok, []} -> {ok, []};
		{error, not_found} -> {ok, []}
	end,
    {reply, Result, State};
handle_call({mapred, BucketKeyPairs}, _From, #state{conn=Conn}=State) ->
	Results = riakc_pb_socket:mapred(Conn, BucketKeyPairs, [
		{map, {modfun, riak_kv_mapreduce, map_object_value}, undefined, false},
		{reduce, {modfun, riak_kv_mapreduce, reduce_set_union}, undefined, true}
	]),
    {reply, Results, State};
handle_call({keys, Bucket}, _From, #state{conn=Conn}=State) ->
	{ok, Results} = riakc_pb_socket:list_keys(Conn, Bucket),
    {reply, {ok, Results}, State};
handle_call(list_buckets, _From, #state{conn=Conn}=State) ->
	case riakc_pb_socket:list_buckets(Conn) of
		{ok, Result} -> {reply, {ok, Result}, State};
		{error, Error} -> {reply, {error, Error}, State}
	end;
handle_call({set_bucket, Bucket, BucketProps}, _From, #state{conn=Conn}=State) ->
	ok = riakc_pb_socket:set_bucket(Conn, Bucket, BucketProps),
    {reply, ok, State};
handle_call({clear, Bucket}, _From, #state{conn=Conn}=State) ->
	{ok, Keys} = riakc_pb_socket:list_keys(Conn, Bucket),
	[riakc_pb_socket:delete(Conn, Bucket, Key) || Key <- Keys],
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=_Conn}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() ->
				case gen_server:start_link({local, ?MODULE},
										   ?MODULE,
										   [{host, "127.0.0.1"}, {port, 8087}],
										   []) of
					{ok, Pid} -> is_pid(Pid);
					{error, {already_started, _}} -> true
				end
		end,
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.

tests(_X) ->
	[
		{"list_buckets",
			fun() ->
					ok
					%io:format(user, "~n~p~n", [ssam_riakc:list_buckets()])
			end
		},
		{"set_bucket",
			fun() ->
					ok = ssam_riakc:set_bucket(<<"m.bucket1">>, [{backend, <<"backend_memory">>}])
			end
		}
	].

-endif.

