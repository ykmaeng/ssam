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

-module(ssam_message_topic).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% Types
-type response_type() :: plain | xml | json. %% | yaml

-type key() :: binary(). 
-type cursor() :: orddict:orddict(). 
-type command() :: subscribe | unsubscribe | publish.
-type transport() :: rest | eventsource | websocket | mqtt | ws_mqtt.
-type action() :: get | post | put | patch | delete.
-type role() :: primary | fallback.
-type node() :: atom().
-type primary() :: node().
-type fallbacks() :: list(node()).
-type path() :: tuple(primary(), fallbacks()).




-type request_props() :: request_subscribe() |
						 request_unsubscribe() |
						 request_publish().

-type request_subscribe() :: subscribe_pid() |
							 subscribe_webhook() |
							 subscribe_email() |
							 subscribe_sms() |
							 subscribe_mms().

-type subscribe_type() :: pid | webhook | email | sms | mms.
-type subscribe_id() :: binary().

-type subscribe_pid() :: list(tuple(atom(), term())) |
						 list(tuple(type, poll) |
							  tuple(from, pid()) |
							  tuple(cursor, ?undefined | binary()) |
							  tuple(last_seq, 0 | pos_integer()) |
							  tuple(count, 0 | pos_integer())).

-type subscribe_webhook() :: list(tuple(atom(), term())) |
							 list(tuple(type, webhook) |
								  tuple(subscriber_id, binary()) |
								  tuple(callback_url, binary()) |
								  tuple(content_type, binary()) |
								  tuple(secret, binary())).

-type subscribe_email() :: list(tuple(atom(), term())).
-type subscribe_sms() :: list(tuple(atom(), term())).
-type subscribe_mms() :: list(tuple(atom(), term())).
							 

-type request_unsubscribe() :: ?undefined.

-type request_publish() :: publish_rest() |
						   publish_eventsource() |
						   publish_websocket() |
						   publish_mqtt() |
						   publish_ws_mqtt().

-type publish_rest() :: list(tuple(atom(), term())) |
						list(tuple(id, pid()) |
							 tuple(body, ?undefined | binary()) |
							 tuple(seq, 0 | pos_integer())).

-type publish_eventsource() :: ?undefined.
-type publish_websocket() :: ?undefined.
-type publish_mqtt() :: ?undefined.
-type publish_ws_mqtt() :: ?undefined.


-export_type([
			  key/0,
			  command/0,
			  cursor/0,
			  transport/0,
			  action/0,
			  role/0,
			  path/0,
			  request_props/0,
			  response_type/0,
			  subscribe_type/0,
			  subscribe_id/0
			 ]).

%% Public
-export([
		 topic/1, topic/2,
		 new/2, new/3,
		 store/1,
		 parts/1,
		 new_path/1,
		 new_primary/1,
		 new_fallbacks/2,
		 keyenc/2,
		 keydec/1,
		 route/2
		]).

-define(MODULE_TOPIC_MANAGER, ssam_message_topic_manager).

topic(Sid, TopicId) ->
	topic(keyenc(Sid, TopicId)).

topic(TopicKey) ->
	case ssam_data:value(?SERVICE, TopicKey) of
		?undefined ->
			?undefined;
		Topic when is_binary(Topic) ->
			binary_to_term(Topic)
	end.

new(Sid, TopicId) ->
	new(Sid, TopicId, default_props()).

new(Sid, TopicId, Props) ->
	Key = keyenc(Sid, TopicId),
	#topic{
	   key = Key,
	   parts = parts(TopicId),
	   path = new_path(Key),
	   fallback_cnt = ?TOPIC_FALLBACK_CNT,
	   props = Props
	}.

store(#topic{key = Key} = Topic) ->
	Bin = term_to_binary(Topic),
	ok = ssam_data:put(?SERVICE, Key, Bin).


new_path(TopicKey) ->
	Primary = new_primary(TopicKey),
	Fallbacks = new_fallbacks(Primary, ?TOPIC_FALLBACK_NODE_CNT),
	lager:debug("~p:new_node -> Primary: ~p, Fallbacks: ~p",
				[?MODULE, Primary, Fallbacks]),
	{Primary, Fallbacks}.

new_primary(TopicKey) ->
	DocIdx = riak_core_util:chash_key({?SERVICE, TopicKey}),
	case riak_core_apl:get_primary_apl(DocIdx, 1, ?RIAK_CORE_RING_NAME) of
		[] -> node();
		[{{_, Node}, primary}] -> Node
	end.

new_fallbacks(Primary, Cnt) ->
	UpNodes = ssam_vnode:up_nodes(),
	_Fallbacks = ssam_vnode:idle_nodes([Primary], UpNodes, Cnt).

keyenc(AccountSid, TopicParts) ->
	term_to_binary({AccountSid, TopicParts}).

keydec(TopicKey) ->
	binary_to_term(TopicKey).

parts(TopicId) ->
	binary:split(TopicId, [<<$:>>, <<$/>>], [global]).

default_props() ->
	[].


route(?subscribe, Req) ->
	route_subscribe(alayze, Req);
route(?unsubscribe, Req) ->
	route_subscribe(alayze, Req);
route(?publish, Req) ->
	route_subscribe(alayze, Req).


route_subscribe(analyze, #topic_request{topic = Topic} = Req) ->
	case any_wildcard(Topic#topic.parts) of
		true ->
			route_subscribe(store, Req);
		false ->
			route_subscribe(route, Req)
	end;
route_subscribe(store, Req) ->
	lager:debug("store");
route_subscribe(route, Req) ->
	lager:debug("route"),
	Res = route_to_primary(Req),
	lager:debug("Res: ~p", [Res]).


any_wildcard(TopicParts) ->
	F = fun(<<$+>>) -> true;
		   (<<$#>>) -> true;
		   (_) -> false
		end,
	lists:any(F, TopicParts).


rpc_first([], _, _) ->
	{?error, ?nodes_not_available};
rpc_first([Node | Rest], Name, Msg) ->
	lager:debug("Node: ~p, Rest: ~p", [Node, Rest]),
	case rpc:multi_server_call([Node], Name, Msg) of
		{[Node], []} ->
			{Node, Rest};
		{[], _Bads} ->
			rpc_first(Rest, Name, Msg)
	end.

rpc_all(Nodes, Name, Msg) ->
	lager:debug("~p:rpc_all -> Nodes: ~p", [?MODULE, Nodes]),
	{ResL, Bads} = rpc:multi_server_call(Nodes, Name, Msg),
	lists:foldl(fun ({?error, Node}, {Goods, Bads1}) ->
						{Goods, [Node | Bads1]};
					(Node, {Goods, Bads1}) ->
						{[Node | Goods], Bads1}
				end, {[], Bads}, ResL).

route_to_primary(#topic_request{topic = Topic} = Req) ->
	{Primary, Fallbacks} = Topic#topic.path,
	case rpc_first([Primary | Fallbacks], ?MODULE_TOPIC_MANAGER, Req) of
		{?error, Reason} ->
			{?error, Reason};
		{Primary, _Rest} ->
			{ok, Req};
		{NewPrimary, Rest} ->
			Topic1 = Topic#topic{path = {NewPrimary, Rest}},
			ssam_message_topic:store(Topic1),
			NewReq = Req#topic_request{topic = Topic1},
			{ok, NewReq}
	end.

route_to_fallbacks(#topic_request{topic = Topic} = Req) ->
	{Primary, Fallbacks} = Topic#topic.path,
	FallbackCnt = Topic#topic.fallback_cnt,
	case rpc_all(Fallbacks, ?MODULE_TOPIC_MANAGER, Req) of
		{Goods, _Bads} when length(Goods) =:= FallbackCnt ->
			{ok, Req};
		{Goods, Bads} ->
			NeedCnt = FallbackCnt - length(Goods),
			NodesExclude = [Primary | Fallbacks],
			case ssam_vnode:idle_nodes(NodesExclude, ssam_vnode:up_nodes(), NeedCnt) of
				[] when Goods =:= [] ->
					store(Topic#topic{path = {Primary, (Fallbacks -- Bads)}}),
					{?error, ?fallback_nodes_not_available};
				[] when length(Goods) > 0 ->
					store(Topic#topic{path = {Primary, (Fallbacks -- Bads)}}),
					{?error, ?fallback_nodes_lack};
				IdleNodes ->
					{ResL1, _Bads} = rpc_all(IdleNodes, ?MODULE_TOPIC_MANAGER, Req),
					Topic1 = Topic#topic{path = (Fallbacks -- Bads) ++ IdleNodes},
					store(Topic1),
					NewReq = Req#topic_request{topic = Topic1},
					{ok, ResL1, NewReq}
			end
	end.







%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() -> ok end,	%% init
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests()
		] end
	}.

tests() ->
	[
		{"test",
		 fun() ->
			ok
		 end
		}
	].

-endif.
