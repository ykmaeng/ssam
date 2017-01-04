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

-module(ssam_message_topic_subscriber).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% APIs
-export([
		 start_link/1,
		 stop/2, stop/3,
		 add/2,
		 all/1,
		 one/2,
		 remove/2,
		 length/1,
		 clear/1
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).


-define(SERVER_TIMEOUT, 30*1000).

-record(state, {
		  topic_key :: ssam_message_topic:key(),
		  server_timeout :: pos_integer()
	}).

%% Public functions

start_link(TopicKey) ->
	gen_server:start_link(?MODULE, [TopicKey], []).

stop(Server, TopicKey) ->
	stop(Server, TopicKey, normal).

stop(Server, TopicKey, Reason) ->
	gen_server:cast(Server, {stop, TopicKey, Reason}).

add(Server, Req) ->
	gen_server:cast(Server, {add, Req}).

remove(Server, ReqId) ->
	gen_server:cast(Server, {remove, ReqId}).

all(TopicKey) ->
	ssam_data:value(bucket(), TopicKey, []).

one(TopicKey, ReqId) ->
	case lists:keysearch(ReqId, 1, all(TopicKey)) of
		false -> ?undef;
		{value, Sub} -> Sub
	end.

length(TopicKey) ->
	erlang:length(all(TopicKey)).

clear(TopicKey) ->
	ssam_data:erase(bucket(), TopicKey).

%% Callback functions

init([TopicKey]) ->
	{ok, #state{topic_key = TopicKey,
				server_timeout = ?SERVER_TIMEOUT},
	 ?SERVER_TIMEOUT}.

handle_call(Unknown, From, State) ->
	lager:debug("Unknown: ~p, From ~p", [Unknown, From]),
    {reply, ok, State, ?TOPIC_SERVER_TIMEOUT}.

handle_cast({add, Req}, State) ->
	{ok, State1} = handle_add(Req#topic_request.body, Req, State),
	{noreply, State1};

handle_cast({remove, ReqId}, State) ->
	TopicKey = State#state.topic_key,
	All = lists:keydelete(ReqId, 1, all(TopicKey)),
	ok = ssam_data:put(bucket(), TopicKey, All),
	{noreply, State};

handle_cast({stop, Reason}, State) ->
	{stop, Reason, State};

handle_cast(Unknown, State) ->
	lager:error("Unknown Cast: ~p, topic_key: ~p", [Unknown, State#state.topic_key]),	
    {noreply, State, ?TOPIC_SERVER_TIMEOUT}.

handle_info(timeout, State) ->
	lager:debug("subscriber timeout. topic_key: ~p", [State#state.topic_key]),
	{noreply, State, ?TOPIC_SERVER_TIMEOUT};

handle_info(Unknown, State) ->
	lager:debug("Unknown Info: ~p, topic_key: ~p", [Unknown, State#state.topic_key]),
	{noreply, State, ?TOPIC_SERVER_TIMEOUT}.

terminate(Reason, _State) ->
	lager:debug("~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

handle_add(#topic_subpid{} = Sub, Req, State) ->
	TopicKey = Req#topic_request.topic#topic.key,
	Subs = ssam_data:value(bucket(), TopicKey, []),
	Subs1 = [subform(Req#topic_request.id, Sub) | Subs],
	ok = ssam_data:put(bucket(), TopicKey, Subs1),
	{ok, State}.
	
bucket() -> ?BUCKET_TOPIC_SUBSCRIBERS.

subform(ReqId, SubX) -> {ReqId, SubX}.


%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() ->
				ok = application:set_env(
					   ssam, riakc, [{host, "127.0.0.1" }, {port, 8087 }]),
				ssam_riakc:start_link(),
				ssam_cache:start_link(),
				Topic = topic(),
				{ok, Pid} = ?MODULE:start_link(Topic#topic.key),
				Pid
		end,
		fun(_X) -> ok end,
		fun(X) -> [
			tests(X)
		] end
	}.

topic() ->
	#topic{key = <<"topic">>}.

request_subpid(Topic, LastSeq, Count) ->
	#topic_request{
		id = <<"reqid">>,
		account_sid = <<"sid">>,
		command = ?subscribe,
		topic = Topic,
		body = #topic_subpid{
				  type = pid,
				  cursor = <<"cursor">>,
				  last_seq = LastSeq,
				  count = Count,
				  stream = no,
				  from = self()
				 }
	}.


tests(Server) ->
	[
		{"subscribe & unsubscribe",
		 fun() ->
				 Topic = topic(),
				 Req = request_subpid(Topic, 0, 0),
				 ok = clear(Topic#topic.key),
				 ok = add(Server, Req),
				 timer:sleep(1),
				 1 = erlang:length(all(Topic#topic.key)),
				 ?assert({Req#topic_request.id, Req#topic_request.body}
						 =:= one(Topic#topic.key, Req#topic_request.id)),
				 ?assert([{Req#topic_request.id, Req#topic_request.body}]
						 =:= all(Topic#topic.key)),
				 ok = remove(Server, Req#topic_request.id),
				 timer:sleep(1),
				 0 = erlang:length(all(Topic#topic.key))
		 end
		}
	].


-endif.
