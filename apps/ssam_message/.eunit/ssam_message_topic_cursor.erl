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

-module(ssam_message_topic_cursor).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

-type key() :: binary(). 

%% APIs
-export([
		 start_link/0,
		 stop/0, stop/1,
		 key/2,
		 cursor/2,
		 new/2, new/3,
		 update/1,
		 delete/1
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).

-export_type([
			  key/0
			 ]).


-define(SERVER_TIMEOUT, 30*1000).

-record(state, {
		  server_timeout :: pos_integer()
	 }).

%% Public functions

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	stop(normal).

stop(Reason) ->
	gen_server:cast(?MODULE, {stop, Reason}).

key(TopicKey, CursorName) ->
	<<"cursor", ?DELIMITER, TopicKey/binary, ?DELIMITER, CursorName/binary>>.

cursor(TopicKey, CursorName) ->
	CursorKey = key(TopicKey, CursorName),
	case ssam_data:value(bucket(), CursorKey) of
		?undef -> new(TopicKey, CursorName);
		Cursor -> Cursor
	end.

new(TopicKey, CursorName) ->
	new(TopicKey, CursorName, 0).

new(TopicKey, CursorName, LastSeq) ->
	CursorKey = key(TopicKey, CursorName),
	Cursor = #topic_cursor{
				key = CursorKey,
				last_seq = LastSeq,
				last_use = ssam_util:now_sec()
			   },
	ok = ssam_data:put(bucket(), CursorKey, Cursor),
	Cursor.

update(Cursor) ->
	Cursor1 = Cursor#topic_cursor{last_use = ssam_util:now_sec()},
	ssam_data:put(bucket(), Cursor1#topic_cursor.key, Cursor1).

delete(Cursor) ->
	ssam_data:delete(bucket(), Cursor#topic_cursor.key).


%% Callback functions

init([]) ->
	{ok, #state{server_timeout = ?SERVER_TIMEOUT}, ?SERVER_TIMEOUT}.

handle_call(Unknown, From, State) ->
	lager:debug("Unknown: ~p, From ~p", [Unknown, From]),
    {reply, ok, State, ?TOPIC_SERVER_TIMEOUT}.

handle_cast({stop, _TopicKey, Reason}, State) ->
	{stop, Reason, State};

handle_cast(Unknown, State) ->
	lager:error("Unknown: ~p", [Unknown]),	
    {noreply, State, ?TOPIC_SERVER_TIMEOUT}.

handle_info(timeout, State) ->
	{noreply, State, ?TOPIC_SERVER_TIMEOUT};

handle_info(_Msg, State) ->
	{noreply, State, ?TOPIC_SERVER_TIMEOUT}.

terminate(Reason, _State) ->
	lager:debug("~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

bucket() -> ?BUCKET_TOPIC_CURSORS.




%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(test, {a, b}).

request_id() -> <<"request_id">>.
topic_key() -> <<"topic_key">>.
cursor_name() -> <<"cursor_name">>.
cursor() -> cursor(topic_key(), cursor_name()).

start_server() ->
	Topic = #topic{key = topic_key()},
	{ok, Pid} = ?MODULE:start_link(Topic),
	{Pid, Topic}.
	


setup_test_() ->
	{setup,
		fun() ->
			ok = application:set_env(ssam, riakc,
									 [{host, "127.0.0.1" }, {port, 8087 }]),
			ssam_riakc:start_link(),
			ssam_cache:start_link()
		end,
		fun(_X) -> ok end,	%% cleanup
		fun(X) -> [		%% tests
			cursor_tests()
		] end
	}.

cursor_tests() ->
	[
		{"cursor_load",
		 fun() ->
				 Cursor = new(topic_key(), cursor_name()),
				 Cursor = cursor(),
				 io:format(user, "~nCursor: ~p~n", [Cursor]),
				 Cursor#topic_cursor{key = topic_key()}
		 end
		},
		{"cursor_update",
		 fun() ->
				 Cursor = cursor(),
				 Cursor1 = Cursor#topic_cursor{last_seq = 10},
				 ok = update(Cursor1),
				 Curosr1 = cursor()
		 end
		}
	].


-endif.



