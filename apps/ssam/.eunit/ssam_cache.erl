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

%% @todo Change the storage from dict to ets

-module(ssam_cache).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").

%% APIs
-export([start_link/0,
		 value/2, value/3,
		 add/3, add/4,
		 add_proplist/2, add_proplist/3,
		 new/3, new/4,
		 put/3, put/4,
		 delete/1, delete/2,
		 size/1,
		 keys/1,
		 buckets/0,
		 to_list/1,
		 global_get/2,
		 global_put/3,
		 global_delete/2,
		 purge/0
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).


-define(buckets, buckets).
-define(PURGE_TIMEOUT_MS, 3600*1000). %% 1 hour
-define(SERVER_TIMEOUT_MS, 60*1000). %% 1 min

%% Be careful when you change this value.
%% It must be matched with expiry time of Riak backend 'memory_day' 
-define(DATA_TIMEOUT_SECS, 86400*1). %% 1 day

-record(data, {
		  value :: ?undefined | term(),
		  ttl :: 0 | pos_integer(),
		  touched :: 0 | ssam:timestamp(),
		  created :: 0 | ssam:timestamp()
		 }).

-record(state, {
		  server_timeout = ?SERVER_TIMEOUT_MS :: non_neg_integer()
		 }).



%% APIs

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

value(Bucket, Key) ->
	value(Bucket, Key, ?undefined).

value(Bucket, Key, Default) ->
	gen_server:call(?MODULE, {value, Bucket, Key, Default}).

add(Bucket, Key, Value) ->
	add(Bucket, Key, Value, default_props()).

add(Bucket, Key, Value, Props) ->
	gen_server:call(?MODULE, {add, Bucket, Key, Value, Props}).

add_proplist(Bucket, Proplist) ->
	add_proplist(Bucket, Proplist, default_props()).

add_proplist(Bucket, Proplist, Props) ->
	gen_server:call(?MODULE, {add_proplist, Bucket, Proplist, Props}).

put(Bucket, Key, Value) ->
	gen_server:call(?MODULE, {put, Bucket, Key, Value, default_props()}).

put(Bucket, Key, Value, Props) ->
	gen_server:call(?MODULE, {put, Bucket, Key, Value, Props}).

new(Bucket, Key, Value) ->
	new(Bucket, Key, Value, []).

new(Bucket, Key, Value, Props) ->
	gen_server:call(?MODULE, {new, Bucket, Key, Value, Props}).

delete(Bucket) ->
	gen_server:call(?MODULE, {delete, Bucket}).

delete(Bucket, Key) ->
	gen_server:call(?MODULE, {delete, Bucket, Key}).

size(Bucket) ->
	gen_server:call(?MODULE, {size, Bucket}).

to_list(Bucket) ->
	gen_server:call(?MODULE, {to_list, Bucket}).

keys(Bucket) ->
	gen_server:call(?MODULE, {keys, Bucket}).

buckets() ->
	gen_server:call(?MODULE, buckets).

global_get(Bucket, Key) ->
	gen_server:call(?MODULE, {global_get, Bucket, Key}).

global_put(Bucket, Key, Value) ->
	gen_server:call(?MODULE, {global_put, Bucket, Key, Value}).

global_delete(Bucket, Key) ->
	gen_server:call(?MODULE, {global_delete, Bucket, Key}).

purge() ->
	gen_server:call(?MODULE, {purge, all}).

%% Callbacks

init([]) ->
	%process_flag(trap_exit, true),	
	erlang:send_after(?PURGE_TIMEOUT_MS, self(), purge),
	{ok, #state{}, ?SERVER_TIMEOUT_MS}.

handle_call({value, Bucket, Key, Default},
			_From, #state{server_timeout = Timeout} = State) ->
	Dict = local_dict(Bucket),
	Result = 
		case dict:find(Key, Dict) of
			{ok, Data} ->
				case is_expired(Data) of
					true -> Default;
					false ->
						Data1 = Data#data{touched = ssam_util:now_sec()},
						Dict1 = dict:store(Key, Data1, Dict),
						put(Bucket, Dict1),
						Data#data.value
				end;
			error ->
				Default
		end,
	{reply, Result, State, Timeout};

handle_call({add, Bucket, Key, Value, Props},
			_From, #state{server_timeout = Timeout} = State) ->
	Dict = dict:append(Key, to_data(Value, Props), local_dict(Bucket)),
	put(Bucket, Dict),
    {reply, ok, State, Timeout};

handle_call({new, Bucket, Key, Value, Props},
			_From, #state{server_timeout = Timeout} = State) ->
	case dict:is_key(Key, Dict = local_dict(Bucket)) of
		false ->
			Dict1 = dict:append(Key, to_data(Value, Props), Dict),
			put(Bucket, Dict1),
			{reply, ok, State, Timeout};
		true ->
			{reply, {?error, ?already_defined}, State, Timeout}
	end;

handle_call({put, Bucket, Key, Value, Props},
			_From, #state{server_timeout = Timeout} = State) ->
	Dict = dict:store(Key, to_data(Value, Props), local_dict(Bucket)),
	put(Bucket, Dict),
    {reply, ok, State, Timeout};

handle_call({delete, Bucket, Key},
			_From, #state{server_timeout = Timeout} = State) ->
	Dict = dict:erase(Key, local_dict(Bucket)),
	put(Bucket, Dict),
    {reply, ok, State, Timeout};

handle_call({delete, Bucket},
			_From, #state{server_timeout = Timeout} = State) ->
	erase(Bucket),
    {reply, ok, State, Timeout};

handle_call({keys, Bucket},
			_From, #state{server_timeout = Timeout} = State) ->
	Keys = dict:fetch_keys(local_dict(Bucket)),
    {reply, Keys, State, Timeout};

handle_call({to_list, Bucket},
			_From, #state{server_timeout = Timeout} = State) ->
	List = dict:to_list(local_dict(Bucket)),
    {reply, List, State, Timeout};

handle_call({add_proplist, Bucket, Proplist, Props},
			_From, #state{server_timeout = Timeout} = State) ->
	F = fun({K, V}, OldDict) ->
			dict:append(K, to_data(V, Props), OldDict)
		end,
	Dict = lists:foldl(F, local_dict(Bucket), Proplist),
	put(Bucket, Dict),
    {reply, ok, State, Timeout};

handle_call(buckets,
			_From, #state{server_timeout = Timeout} = State) ->
	Buckets = get(?buckets),
    {reply, Buckets, State, Timeout};

handle_call({global_get, Bucket, Key},
			_From, #state{server_timeout = Timeout} = State) ->
	Key1 = term_to_binary({Bucket, Key}),
	Result = ssam_riakc:get(?BUCKET_CACHE, Key1),
    {reply, Result, State, Timeout};

handle_call({global_put, Bucket, Key, Value},
			_From, #state{server_timeout = Timeout} = State) ->
	Key1 = term_to_binary({Bucket, Key}),
	Result = ssam_riakc:put(?BUCKET_CACHE, Key1, Value),
    {reply, Result, State, Timeout};

handle_call({global_delete, Bucket, Key},
			_From, #state{server_timeout = Timeout} = State) ->
	Key1 = term_to_binary({Bucket, Key}),
	Result = ssam_riakc:delete(?BUCKET_CACHE, Key1),
    {reply, Result, State, Timeout};

handle_call({purge, all},
			_From, #state{server_timeout = Timeout} = State) ->
	Result = purge(all),
    {reply, Result, State, Timeout};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(purge, #state{server_timeout = Timeout} = State) ->
	lager:debug("purge, Timeout: ~p", [?PURGE_TIMEOUT_MS]),
	purge(all),
	erlang:send_after(?PURGE_TIMEOUT_MS, self(), purge),
    {noreply, State, Timeout};

handle_info({reload, Bucket, Key}, #state{server_timeout = Timeout} = State) ->
	lager:debug("reload, Bucket: ~p, Key: ~p", [Bucket, Key]),
	Dict = dict:erase(Key, local_dict(Bucket)),
	put(Bucket, Dict),
	{noreply, State, Timeout};

handle_info(timeout, State) ->
	lager:debug("timeout, hibernate"),
	purge(all),
    {noreply, State, hibernate};

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:warning("~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Private functions

purge(all) ->
	F = fun(Bucket) ->
		{ok, Dict} = purge(local_dict(Bucket)),
		case dict:size(Dict) > 0 of
			true ->
				put(Bucket, Dict);
			false ->
				delete_bucket(Bucket)
		end
	end,
	case get(?buckets) of
		undefined ->
			put(?buckets, []);
		Buckets ->
			lists:foreach(F, Buckets)
	end;
purge(Dict) ->
	F = fun(_, #data{ttl = TTL, touched = Touched}) ->
			not ssam_util:is_expired(Touched, TTL)
		end,
	Dict1 = dict:filter(F, Dict),
	{ok, Dict1}.

add_bucket(Bucket) ->
	case get(?buckets) of
		undefined ->
			put(?buckets, [Bucket]);
		Buckets ->
			put(?buckets, (Buckets ++ [Bucket]))
	end.

delete_bucket(Bucket) ->
	case get(?buckets) of
		undefeind ->
			ok;
		Buckets ->
			put(?buckets, (Buckets -- [Bucket]))
	end.

local_dict(Bucket) ->
	case get(Bucket) of
		?undefined ->
			put(Bucket, Dict = dict:new()),
			add_bucket(Bucket),
			Dict;
		Dict ->
			Dict
	end.

to_data(Value, Props) ->
	Now = ssam_util:now_sec(),
	#data{value = Value,
		  ttl = ssam_objects:value(ttl, Props, ?DATA_TIMEOUT_SECS),
		  touched = Now,
		  created = Now}.

is_expired(#data{ttl = TTL, touched = Touched}) ->
	ssam_util:is_expired(Touched, TTL).

default_props() ->
	[{ttl, ?DATA_TIMEOUT_SECS}].

%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() -> ok end,		%% init 
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.

tests(_X) ->
	[
		{"whatever",
			fun() ->
				undef
			end
		}
	].

-endif.
