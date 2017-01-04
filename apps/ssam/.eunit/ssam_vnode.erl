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

-module(ssam_vnode).

-behaviour(riak_core_vnode).

-export([start_vnode/1,
		 service_up/0,
		 route/3,
		 route/4,
		 local_route/3,
		 idle_nodes/3,
		 up_nodes/0
		]).


-export([init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-include("ssam.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-record(state, {
		  partition :: integer()
		 }).

%% APIs

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

service_up() ->
	ok = riak_core_ring_events:add_guarded_handler(ssam_ring_event_handler, []),
	ok = riak_core_node_watcher_events:add_guarded_handler(ssam_node_event_handler, []),
	ok = riak_core:register(ssam, [{vnode_module, ssam_vnode}]),
	ok = riak_core_node_watcher:service_up(ssam, self()).

local_route(Module, Fun, Request) ->
	invoke(Module, Fun, Request, self()).

route(Module, Fun, #request{service = Service, path = Path} = Request) ->
	riak_core_vnode_master:sync_spawn_command(preferred_node(Service, Path),
											  {Module, Fun, Request},
											  ssam_vnode_master).

route(Module, Fun, Request, {Bucket, Key}) ->
	PrefNode = preferred_node(Bucket, Key),
	riak_core_vnode_master:sync_spawn_command(PrefNode,
											  {Module, Fun, Request},
											  ssam_vnode_master).

idle_nodes([], Nodes, Len) ->
	lists:sublist(Nodes, Len);
idle_nodes([Except | Rest], Nodes, Len) ->
	Fun = fun(Node) -> Node =/= Except end,
	case lists:splitwith(Fun, Nodes) of
		{Takes, []} -> idle_nodes(Rest, Takes, Len);
		{Takes, [_ | Drops]} -> idle_nodes(Rest, Drops++Takes, Len)
	end.

up_nodes() ->
	riak_core_node_watcher:nodes(?RIAK_CORE_RING_NAME).


%% Callback functions

init([Partition]) ->
	lager:debug("Pid: ~p, Partition: ~p", [self(), Partition]),
    {ok, #state{partition = Partition}}.

handle_command({Module, Fun, Request}, Sender,
			   State = #state{partition = Partition}) ->
    lager:debug("Module: ~w, Method: ~w, Request: ~p, Partition: ~w",
				[Module, Fun, Request, Partition]),
	Result = invoke(Module, Fun, Request, Sender),
    {reply, Result, State};
handle_command(Request, _Sender,
			   State = #state{partition = Partition}) ->
    lager:debug("Request: ~p, Partition: ~w", [Request, Partition]),
    {reply, {error, unknown_request}, State}.

handle_handoff_command(Message = #riak_core_fold_req_v1{foldfun=_Fun, acc0=_Acc0}, _Sender,
					   State = #state{partition = Partition}) ->
    lager:debug("Message: ~p, Partition: ~w", [Message, Partition]),
    {noreply, State}.

handoff_starting(TargetNode,
				 State = #state{partition = Partition}) ->
    lager:debug("TargetNode: ~p, Partition: ~w", [TargetNode, Partition]),
    {true, State}.

handoff_cancelled(State = #state{partition = Partition}) ->
    lager:debug("Partition: ~w", [Partition]),
    {ok, State}.

handoff_finished(TargetNode, State = #state{partition = Partition}) -> 
    lager:debug("TargetNode: ~w, Partition: ~w", [TargetNode, Partition]),
    {ok, State}.

handle_handoff_data(Data, State = #state{partition = Partition}) -> 
	DataBin = binary_to_term(Data)	,
    lager:debug("DataBin: ~p, Partition: ~w", [DataBin, Partition]),
    {reply, ok, State}.

encode_handoff_item(ObjectName, ObjectValue) ->
    lager:debug("ObjectName: ~p, ObjectValue: ~p", [ObjectName, ObjectValue]),
    term_to_binary({ObjectName, ObjectValue}).

is_empty(State = #state{partition = Partition}) ->
    lager:debug("Partition: ~p", [Partition]),
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%% Private functions

preferred_node(Bucket, Key) ->
	Idx = riak_core_util:chash_key({Bucket, Key}),
    [PrefNode] = riak_core_apl:get_apl(Idx, 1, ?RIAK_CORE_RING_NAME),
	PrefNode.

invoke(Module, Fun, Request, Sender) ->
	lager:debug("Module: ~w, Fun: ~w, Sender: ~w", [Module, Fun, Sender]),
	Module:Fun(Request, Sender).

