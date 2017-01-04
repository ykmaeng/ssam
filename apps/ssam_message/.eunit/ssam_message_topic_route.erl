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

-module(ssam_message_topic_route).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").


%% APIs
-export([dispatch/1]).


-define(MODULE_TOPIC_MANAGER, ssam_message_topic_manager).

%% Public functions

dispatch(Req) ->
	_Pid = spawn(fun() -> handle_dispatch(Req) end),
	ok.


%% Private functions

handle_dispatch(#topic_request{command = ?subscribe} = Req) ->
	lager:debug("subscribe!"),
	state_subscribe(analyze, Req);
handle_dispatch(#topic_request{command = ?unsubscribe} = Req) ->
	state_subscribe(analyze, Req);
handle_dispatch(#topic_request{command = ?publish} = Req) ->
	state_subscribe(analyze, Req).

state_subscribe(analyze, #topic_request{topic = Topic} = Req) ->
	case any_wildcard(Topic#topic.parts) of
		true ->
			state_subscribe(store_subscriber, Req);
		false ->
			state_subscribe(route_to_primary, Req)
	end;
state_subscribe(store_subscriber, Req) ->
	lager:debug("store_subscriber");
state_subscribe(route_to_primary, Req) ->
	lager:debug("route_to_primary"),
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
					ssam_message_topic:store(
					  Topic#topic{path = {Primary, (Fallbacks -- Bads)}}),
					{?error, ?fallback_nodes_not_available};
				[] when length(Goods) > 0 ->
					ssam_message_topic:store(
					  Topic#topic{path = {Primary, (Fallbacks -- Bads)}}),
					{?error, ?fallback_nodes_lack};
				IdleNodes ->
					{ResL1, _Bads} = rpc_all(IdleNodes, ?MODULE_TOPIC_MANAGER, Req),
					Topic1 = Topic#topic{path = (Fallbacks -- Bads) ++ IdleNodes},
					ssam_message_topic:store(Topic1),
					NewReq = Req#topic_request{topic = Topic1},
					{ok, ResL1, NewReq}
			end
	end.


