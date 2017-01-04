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

-module(ssam_message_topic_manager).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% APIs
-export([start_link/0,
		 remove_topic_server/1
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).

-define(MODULE_TOPIC_SERVER, ssam_message_topic_server).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callback functions

init([]) ->
	process_flag(trap_exit, true),
	{ok, []}.

handle_call(Request, From, State) ->
	lager:warning("Request: ~p, From ~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
	lager:warning("Request: ~p", [Request]),	
    {noreply, State}.

handle_info({From, Request} = Msg, State) ->
	lager:debug("Msg: ~p", [Msg]),
	spawn(fun() ->
				  Res = handle_request(Request),
				  reply_to_router(From, Res)
		  end),
	{noreply, State};
handle_info(Unknown, State) ->
	lager:error("Unknown: ~p", [Unknown]),
	{noreply, State}.

terminate(Reason, _State) ->
	lager:debug("~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions 

handle_request(#topic_request{command = subscribe} = Req) ->
	lager:debug("subscribe"),
	state_subscribe(primary, Req);

handle_request(#topic_request{command = unsubscribe} = _Req) ->
	lager:debug("unsubscribe"),
	ok;

handle_request(#topic_request{command = publish} = _Req) ->
	lager:debug("publish"),
	ok.

state_subscribe(primary, Req) ->
	Topic = Req#topic_request.topic,
	Pid = forced_topic_server(Topic, primary),
	case catch ssam_message_topic_server:subscribe(Pid, Req) of
		ok -> node();
		_Error -> {?error, node()}
	end.

forced_topic_server(Topic, Role) ->
	case topic_server(Topic#topic.key) of
		?undef ->
			{ok, Pid} = ssam_message_topic_server:start_link(Topic, Role),
			ok = ssam_data:put(bucket_topic_server(), Topic#topic.key, Pid),
			Pid;
		Pid ->
			Pid
	end.

stop_topic_server(TopicKey) ->
	case topic_server(TopicKey) of
		?undef -> ?undef;
		Server -> ssam_message_topic_server:stop(Server, TopicKey)
	end.

remove_topic_server(TopicKey) ->
	ssam_data:erase(bucket_topic_server(), TopicKey).

topic_server(TopicKey) ->
	ssam_data:value(bucket_topic_server(), TopicKey).

bucket_topic_server() ->
	?BUCKET_TOPIC_SERVER.

reply_to_router(From, Msg) ->
	From ! {?MODULE, node(), Msg}.







-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() -> 
			Topic = topic()
		end,	%% initialize
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.


tests(_X) ->
	[
		{"server_pid",
			fun() ->
					Topic = topic(),
					Pid = forced_server_pid(Topic, primary),
					?assert(is_pid(Pid))
			end
		},
		{"handle_request(subscribe)",
			fun() ->
					Topic = topic(),
					Req = request_subpid(Topic, 0, 0),
					ok = handle_request(Req)
			end
		}
	].

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

-endif.






