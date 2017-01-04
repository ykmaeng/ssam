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

-module(ssam_message_topic_dispatcher).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% APIs
-export([
		 start_link/1,
		 stop/2, stop/3,
		 send/3,
		 request/2
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

send(Server, Msgs, Targets) ->
	gen_server:call(Server, {send, Msgs, Targets}).

request(Server, Request) ->
	gen_server:call(Server, {request, Request}).

%% Callback functions

init([TopicKey]) ->
	{ok, #state{topic_key = TopicKey,
				server_timeout = ?SERVER_TIMEOUT}, ?SERVER_TIMEOUT}.

handle_call({send, Msgs, Targets}, _From, State) ->
	lager:debug("send, Msgs: ~p, Targets: ~p", [Msgs, Targets]),
    {reply, ok, State, ?TOPIC_SERVER_TIMEOUT};

handle_call({request, Req}, _From, State) ->
	lager:debug("request, Req: ~p", [Req]),
    {reply, ok, State, ?TOPIC_SERVER_TIMEOUT};

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



