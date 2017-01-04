-module(ssam_telephony_dialplan).

-export([
		 start/0,
		 reload/0,
		 run/1,
		 launch/1,
		 send_msg/3, send_msg/4,
		 send_msg_locked/4
		]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").


-record(state, {
		  channel = #telephony_channel{} :: record()
		 }).


start()->
	%% start our handler process running
	Pid = spawn(?MODULE, run, [initial_state()]),
	%% register it with the same name as the module - myhandler
	register(?MODULE, Pid).

reload() ->
	?MODULE ! reload.

run(State)->
	%% wait for messages from FreeSWITCH
	receive
		{call, Data}->
			{event, [UUID | Rest]} = Data,
			lager:debug("~p:run -> call, UUID: ~p",[?MODULE, UUID]),
			%lager:debug("~p:run -> call, Data:~n~p",[?MODULE, Data]),
			Channel = State#state.channel#telephony_channel{uuid = UUID, data = Rest},
			run(State#state{channel = Channel});
		{call_event, Data} ->
			{event, [_UUID | Rest]} = Data,
			Event = proplists:get_value(<<"Event-Name">>, Rest),
			case handle_event(Event, Rest, State) of
				{ok, State1} -> run(State1);
				{ignored, State1} -> run(State1);
				{hangup, State1} -> {ok, State1};
				{stop, State1} -> {ok, State1}
			end;
		{call_hangup, Data} ->
			{event, [UUID | _Rest]} = Data,
			lager:debug("~p:run -> call, UUID: ~p",[?MODULE, UUID]),
			{ok, State};
		{get_pid, UUID, Ref, Pid} ->
			%% request from FreeSWITCH for an outbound process to handle call at 'UUID'
			NewPid = spawn(?MODULE, run, [State]),
			lager:debug("~p:run -> get_pid, UUID: ~p, NewPid: ~p", [?MODULE, UUID, NewPid]),
			Pid ! {Ref, NewPid},
			run(State);
		reload ->
			run(State);
		Unknown ->
			lager:warning("~p:run -> Unknown Event: ~p", [?MODULE, Unknown]),
			run(State)
	end.

handle_event(<<"CHANNEL_PARK">> = Event, Props,
			 #state{channel = Channel} = State) ->
	#telephony_channel{uuid = UUID} = Channel,
	lager:debug("~p:handle_event -> ~p, ~p", [?MODULE, UUID, Event]),

	AccId = proplists:get_value(<<"variable_acc_id">>, Props, <<>>),
	AppId = proplists:get_value(<<"variable_app_id">>, Props, <<>>),

	case ssam_riakc:get(<<?SERVICE/bits, "_calls">>,
						<<AccId/bits, ?DELIMITER, AppId/bits>>) of
		{ok, Bin} when is_binary(Bin) ->
			Doc = binary_to_term(Bin),
			io:format("~n~n######### Doc ###########~n~p~n~n", [Doc]),
			App = proplists:get_value(app, Doc),
			Vars = proplists:get_value(vars, Doc),
			ssam_aml:start(App, #ssaml{vars = Vars, state = Channel}),
			{ok, State};
		{?error, Reason} ->
			lager:error("~p:handle_event -> ~p", [?MODULE, Reason]),
			{ok, State}
	end;
handle_event(<<"CHANNEL_HANGUP">> = Event, _Props,
			 #state{channel = Channel} = State) ->
	#telephony_channel{uuid = UUID} = Channel,
	lager:debug("~p:handle_event -> ~p, ~p", [?MODULE, UUID, Event]),
	{hangup, State};
handle_event(Event, _Props, #state{channel = Channel} = State) ->
	%lager:debug("~p:handle_event -> ~p, ~p ignored", [?MODULE, UUID, Event]),
	#telephony_channel{uuid = UUID} = Channel,
	%io:format("~n~p:handle_event -> ~p, ~p ignored~n~p~n", [?MODULE, UUID, Event, Props]),
	lager:debug("~p:handle_event -> ~p, ~p ignored", [?MODULE, UUID, Event]),
	{ignored, State}.

launch(Ref) ->
	%% rpc call to a function to return a new outbound call pid
	NewPid = spawn(?MODULE, run, [initial_state()]),
	lager:debug("~p:launch -> NewPid: ~p", [?MODULE, NewPid]),
	{Ref, NewPid}.

initial_state() ->
	NodeFs = ssam_config:value(?SERVICE, [freeswitch, node_fs]),
	#state{channel = #telephony_channel{node_fs = list_to_atom(NodeFs)}}.

send_msg(Node, UUID, App, ArgStr) ->
	Headers = [{"call-command", "execute"},
		{"execute-app-name", atom_to_list(App)}, {"execute-app-arg", ArgStr}],
	send_msg(Node, UUID, Headers).

send_msg_locked(Node, UUID, App, ArgStr) ->
	Headers = [{"call-command", "execute"}, {"event-lock", "true"},
		{"execute-app-name", atom_to_list(App)}, {"execute-app-arg", ArgStr}],
	send_msg(Node, UUID, Headers).

send_msg(Node, UUID, Headers) -> {fs, Node} ! {sendmsg, UUID, Headers}.
	
