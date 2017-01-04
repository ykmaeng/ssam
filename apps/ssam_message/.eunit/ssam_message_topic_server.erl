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

-module(ssam_message_topic_server).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% APIs
-export([
		 start_link/2,
		 stop/2, stop/3,
		 subscribe/2,
		 unsubscribe/2,
		 publish/2,
		 reload/2,
		 bytes/2,
		 length/2,
		 clean/2,
		 set_role/3
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).

-record(state, {
		  topic :: ?undef | #topic{},
		  role :: ?primary | ?fallback,
		  msgs = [] :: [] | list(),
		  last_seq = 0 :: 0 | pos_integer(),
		  subscriber :: pid(),
		  dispatcher :: pid()
		 }).

-define(TOPIC_SENDMSG_TIMEOUT, 3*1000).

-define(BUCKET_MESSAGE_TOPIC, <<"ssam_message_topic">>). 
-define(BUCKET_MESSAGE_TOPIC_SUB, <<?BUCKET_MESSAGE_TOPIC/bits, 1>>). 
-define(BUCKET_MESSAGE_TOPIC_CURSOR, <<?BUCKET_MESSAGE_TOPIC/bits, 2>>). 


%% Public functions

start_link(Topic, Role) ->
	gen_server:start_link(?MODULE, [Topic, Role], []).

stop(Server, TopicKey) ->
	stop(Server, TopicKey, normal).

stop(Server, TopicKey, Reason) ->
	gen_server:cast(Server, {stop, TopicKey, Reason}).

subscribe(Server, Req) ->
	Key = Req#topic_request.topic#topic.key,
	gen_server:call(Server, {Key, {subscribe, Req}}).

unsubscribe(Server, Req) ->
	Key = Req#topic_request.topic#topic.key,
	gen_server:call(Server, {Key, {unsubscribe, Req}}).

publish(Server, Req) ->
	Key = Req#topic_request.topic#topic.key,
	gen_server:call(Server, {Key, {publish, Req}}).

reload(Server, Topic) ->
	Key = Topic#topic.key,
	gen_server:call(Server, {Key, {reload, Topic}}).

bytes(Server, TopicKey) ->
	gen_server:call(Server, {TopicKey, bytes}).

length(Server, TopicKey) ->
	gen_server:call(Server, {TopicKey, length}).

clean(Server, TopicKey) ->
	gen_server:call(Server, {TopicKey, clean}).

set_role(Server, TopicKey, Role) ->
	gen_server:call(Server, {TopicKey, {set_role, Role}}).


%% Callback functions

init([Topic, Role]) ->
	{Subscriber, Dispatcher} =
		case Role =:= ?primary of
			true ->
				{ok, SubPid} = ssam_message_topic_subscriber:start_link(Topic),
				{ok, PubPid} = ssam_message_topic_dispatcher:start_link(Topic),
				{SubPid, PubPid};
			false ->
				{?undef, ?undef}
		end,
	{ok, #state{topic = Topic, role = Role, last_seq = 0,
				subscriber = Subscriber, dispatcher= Dispatcher},
	 ?TOPIC_SERVER_TIMEOUT}.

handle_call({TopicKey, Req}, From, State)
	when TopicKey =:= State#state.topic#topic.key ->
	lager:debug("TopicKey: ~p, Req: ~p, From ~p", [TopicKey, Req, From]),
	{Res, State1} = handle_request(Req, From, State),
    {reply, Res, State1};

handle_call({TopicKey, _}, From, State) ->
	lager:error("TopicKey not matched! '~p' : '~p', From ~p",
				[TopicKey, State#state.topic#topic.key, From]),
    {reply, {error, ?topic_key_not_matched}, State}.

handle_cast({stop, TopicKey, Reason}, State)
	when TopicKey =:= State#state.topic#topic.key ->
	{stop, Reason, State};

handle_cast(Unknown, State) ->
	lager:error("Unknown: ~p", [Unknown]),	
    {noreply, State}.

handle_info(timeout, State) ->
	ok = purge(),
	case length() of
		0 ->
			lager:info("~p:handle_info -> timeout, stop", [?MODULE]),
			{stop, {shutdown, timeout}, State};
		N when N > 0 ->
			{noreply, State, ?TOPIC_SERVER_TIMEOUT}
	end;

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(Reason, #state{topic = Topic}) ->
	lager:debug("~p, TopicKey: ~p", [Reason, Topic#topic.key]),
	ok = remove(Topic#topic.key).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

handle_request({subscribe, Req}, _From, State) ->
	Type = ssam_objects:value(type, Req#topic_request.body),
	{Res, State1} = subscribe(Type, Req, State),
	{reply, Res, State1};

handle_request({publish, Req}, _From, State) ->
	%{Pub, Extras} = ?props_to_record(Req#topic_request.body, topic_publish),
	%Pub1 = Pub#topic_publish{extras = Extras},
	Res = state_publish(append, {Req, State}),
	{reply, Res, State};

handle_request({reload, Topic}, _From, State) ->
    {reply, ok, State#state{topic = Topic}};

handle_request(bytes, _From, State) ->
    {reply, bytes(), State};

handle_request(length, _From, State) ->
    {reply, length(), State};

handle_request(clean, _From, State) ->
    {reply, clean(), State};

handle_request({set_role, Role}, _From, State) ->
	State1 = State#state{role = Role},
    {reply, ok, State1};

handle_request(Unknown, From, State) ->
	lager:error("Unknown: ~p, From: ~p", [Unknown, From]),	
    {reply, ok, State}.

subscribe(pid, #topic_request{body = Props} = Req, State) ->
	{Record, Extras} = ?props_to_record(Props, topic_subpid),
	Record1 = Record#topic_subpid{extras = Extras},
	Req1 = Req#topic_request{body = Record1},
	state_subscribe_pid(get, {Req1, State}).

state_subscribe_pid(get, {Req, State}) ->
	Body = Req#topic_request.body,
	Cursor =
		case Body#topic_subpid.cursor of
			?undef -> ?undef;
			Name -> ssam_message_topic_cursor:cursor(
					  State#state.topic#topic.key, Name)
		end,
	LastSeq = Body#topic_subpid.last_seq,
	Count = Body#topic_subpid.count,
	Res = get(Cursor, LastSeq, Count, State),
	state_subscribe_pid(Res, {Req, State});
state_subscribe_pid(none, {_Req, State}) ->
	{ok, State};
state_subscribe_pid(wait, {Req, State}) ->
	Server = State#state.subscriber,
	ok = ssam_message_topic_subscriber:add(Server, Req),
	{ok, State};
state_subscribe_pid({ok, Msgs}, {Req, State}) ->
	Dispatcher = State#state.dispatcher,
	Body = Req#topic_request.body,
	Targets = [Body#topic_subpid.from],
	ok = ssam_message_topic_dispatcher:send(Dispatcher, Msgs, Targets),
		   %State#state.topic, Msgs, State),
	{ok, State}.

state_publish(append, {Req, State}) ->
	Msg =
		#topic_message{
		   topic_key = State#state.topic#topic.key,
		   seq = State#state.last_seq + 1,
		   msg_id = Req#topic_request.id,
		   msg_body = Req#topic_request.body,
		   timestamp = Req#topic_request.timestamp
		},
	Msgs = [Msg | State#state.msgs],
	Req1 = Req#topic_request{body = Msg},
	State1 = State#state{msgs = Msgs},
	state_publish(send, {Req1, State1});
state_publish(send, {Req, State}) ->
	Dispatcher = State#state.dispatcher,
	Res = ssam_message_topic_diapatcher:request(Dispatcher, Req),
	{Res, State}.

get(?undef, 0, 0, _State) ->
	wait;
get(Cursor, 0, 0, State) ->
	case Cursor#topic_cursor.last_seq of
		0 -> wait;
		LastSeq -> get(Cursor, LastSeq, 0, State)
	end;
get(?undef, LastSeq, 0, State) when LastSeq > 0 ->
	case State#state.last_seq of
		LastSeq -> wait;
		N when LastSeq > N -> none;
		N when LastSeq < N ->
			case select(LastSeq, 1, State#state.msgs) of
				[] -> none;
				Msgs -> {ok, Msgs}
			end
	end;
get(?undef, 0, Count, State) when Count > 0 ->
	Msgs = select(0, Count, State#state.msgs),
	{ok, Msgs};
get(Cursor, LastSeq, 0, State) when LastSeq > 0 ->
	get(Cursor, LastSeq, 1, State);
get(Cursor, 0, Count, State) ->
	LastSeq = Cursor#topic_cursor.last_seq,
	case State#state.last_seq of
		LastSeq -> wait;
		N when LastSeq > N -> none;
		N when LastSeq < N ->
			case select(LastSeq, Count, State#state.msgs) of
				[] -> none;
				[Head | Rest] ->
					Seq = Head#topic_message.seq,
					ssam_message_topic_cursor:update(Cursor#topic_cursor{last_seq = Seq}),
					{ok, [Head | Rest]}
			end
	end;
get(?undef, LastSeq, Count, State) when LastSeq > 0, Count > 0 ->
	case select(LastSeq, Count, State#state.msgs) of
		[] -> none;
		Msgs -> {ok, Msgs}
	end;
get(Cursor, LastSeq, Count, State) when LastSeq > 0, Count > 0 ->
	case Cursor#topic_cursor.last_seq =:= LastSeq of
		true ->
			case select(LastSeq, Count, State#state.msgs) of
				[] -> none;
				[Head | Rest] ->
					Seq = Head#topic_message.seq,
					ssam_message_topic_cursor:update(Cursor#topic_cursor{last_seq = Seq}),
					{ok, [Head | Rest]}
			end;
		false ->
			case State#state.last_seq of
				LastSeq ->
					Cursor1 = Cursor#topic_cursor{last_seq = LastSeq},
					ssam_message_topic_cursor:update(Cursor1),
					wait;
				N when LastSeq > N -> none;
				N when LastSeq < N ->
					case select(LastSeq, 1, State#state.msgs) of
						[] ->
							ssam_message_topic_cursor:update(
							  Cursor#topic_cursor{last_seq = LastSeq}),
							none;
						[Head | Rest] ->
							Seq = Head#topic_message.seq,
							ssam_message_topic_cursor:update(
							  Cursor#topic_cursor{last_seq = Seq}),
							{ok, [Head | Rest]}
					end
			end
	end.


select(0, Count, L) when Count > 0 ->
	lists:sublist(L, 1, Count);
select(LastSeq, Count, L) when LastSeq >= 0, Count >= 0 ->
	L2 = lists:takewhile(fun(Msg) -> Msg#topic_message.seq > LastSeq end, L),
	case (length(L2) - Count) of
		N when N < 0 -> lists:nthtail(0, L2);
		N -> lists:nthtail(N, L2)
	end;
select(_, _, _) ->
	[].


purge() ->
	ok.


bytes() ->
	0.

length() ->
	0.

clean() ->
	ok.






%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(test, {a :: term(), b :: term()}).

start_server() ->
	Topic = #topic{key = <<"topic">>},
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
		fun(_X) -> [		%% tests
			record_tests(),
			message_tests()
			%server_tests()
		] end
	}.

record_tests() ->
	[
		{"record_to_props",
		 fun() ->
			Test = #test{a = 1},
			Props = [{a, 1}, {b, undefined}] ,
			Props = ?record_to_props(Test, test)
		 end
		},
		{"props_to_record",
		 fun() ->
			Test = #test{a = 1},
			Props = [{a, 1}, {b, undefined}],
			{Test, []}= ?props_to_record(Props, test),
			Props1 = [{c, 1} | Props],
			{Test, [{c, 1}]}= ?props_to_record(Props1, test)
		 end
		}
	].

message_tests() ->
	[
		{"select",
		 fun() ->
				 SampleMsgs =
					 fun(Cnt) ->
						 lists:reverse([#topic_message{seq = N, msg_id = N, msg_body = N}
										|| N <- lists:seq(1, Cnt)])
					 end,
				 L = SampleMsgs(10),
				 [] = select(0, 0, L),
				 [] = select(1000, 0, L),
				 [] = select(1000, 1000, L),
				 [] = select(0, -100, L),
				 [] = select(-100, 0, L),
				 [] = select(-100, -100, L),
				 [] = select(0, 1, []),
				 [#topic_message{seq = 10}] = select(0, 1, L),
				 [#topic_message{seq = 2}] = select(1, 1, L),
				 L = select(0, 1000, L)
		 end
		}
	].


server_tests() ->
	[
		{"subscribe",
		 fun() ->
				 Req =
					fun(Topic, LastSeq, Count) ->
						 #topic_request{
							id = <<"request_id">>,
							account_sid = <<"account_sid">>,
							command = ?subscribe,
							topic = Topic,
							body = [{type, pid},
									 {cursor, <<"cursor_name">>},
									 {last_seq, LastSeq},
									 {count, Count},
									 {stream, no},
									 {from, self()}
									]
						   }
					end,
				{Pid, Topic} = start_server(),
				none = ?MODULE:subscribe(Pid, Req(Topic, 0, 1))
		 end
		}
	].

-endif.




