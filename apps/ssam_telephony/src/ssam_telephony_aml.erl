-module(ssam_telephony_aml).

-compile(export_all).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").

app_answer(Attrs, Data, State) ->
	lager:debug("~p:app_answer -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	{next, State}.

app_hangup(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_hangup -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	Res = send_msg(Node, UUID, hangup),
	lager:debug("~p:handle_event -> send_msg(sleep) Res: ~p", [?MODULE, Res]),
	{next, State}.

app_play(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_playback -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	case proplists:get_value("terminators", Attrs) of
		?undefined -> ?undefined;
		"" -> send_msg(Node, UUID, set, "playback_terminators=none");
		Terms -> send_msg(Node, UUID, set, "playback_terminators="++Terms)
	end,
	Arg = "ivr/" ++ Data ++ ".wav", %% @todo 
	Res = send_msg_locked(Node, UUID, playback, Arg),
	lager:debug("~p:handle_event -> send_msg_locked(playback) Res: ~p", [?MODULE, Res]),
	wait_for_complete(State),
	{next, State}.


app_say(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_say -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	case proplists:get_value("terminators", Attrs, "") of
		"" -> send_msg(Node, UUID, set, "playback_terminators=none");
		Terms -> send_msg(Node, UUID, set, "playback_terminators="++Terms)
	end,
	%Data1 = http_uri:encode(Data),
	Data1 = http_uri:encode(Data),
	Lang = case proplists:get_value("lang", Attrs, "") of
		"" -> "en"; Lang_ -> Lang_
    end,
	Arg = "shout://translate.google.com/translate_tts?tl=" ++ Lang ++ "&q=" ++ Data1,
	Res = send_msg_locked(Node, UUID, playback, Arg),
	lager:debug("~p:handle_event -> send_msg_locked(say) Res: ~p", [?MODULE, Res]),
	wait_events([<<"PLAYBACK_STOP">>], State),
	{next, State}.


app_input(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_input -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	Lang = proplists:get_value("lang", Attrs, "en"),
	Min = proplists:get_value("min", Attrs, "0"),
	Max = proplists:get_value("max", Attrs, "128"),
	Play = case proplists:get_value("file", Attrs, "") of
		"" ->
			case proplists:get_value("say", Attrs, "") of
				"" -> "tone_stream://L=100;%(100,100,350,440)";
				Play_ -> "shout://translate.google.com/translate_tts?tl="
						 ++ Lang ++ "&q=" ++ http_uri:encode(Play_)
			end;
		Play_ ->
			Play_
	end,
	Var = proplists:get_value("var", Attrs, "input"),
	Var1 = "tellet_" ++ Var,
	Timeout = proplists:get_value("timeout", Attrs, "5000"),
	Terminators = proplists:get_value("terminators", Attrs, "#"),
	Arg = string:join([Min, Max, Play, Var1, Timeout, Terminators], " "),
	Res = send_msg_locked(Node, UUID, read, Arg),
	lager:debug("~p:handle_event -> send_msg_locked(read) Res: ~p", [?MODULE, Res]),
	case wait_events([<<"DTMF">>, <<"CHANNEL_HANGUP">>], State) of
		{ok, {<<"DTMF">>, _, State}} ->
			case wait_for_complete(State) of
				{ok, {<<"CHANNEL_EXECUTE_COMPLETE">>, Props, State}} ->
					Input = proplists:get_value(
							  <<"variable_", (list_to_binary(Var1))/bits>>, Props, <<>>),
					Vars = [{Var, Input} | State#ssaml.vars],
					lager:debug("~p:app_input -> ~p: ~p, Vars: ~p", [?MODULE, Var, Input, Vars]),
					State1 = State#ssaml{vars = Vars},	
					{continue, Data, State1};
				_ ->
					{stop, State}
			end;
		_ ->
			{stop, State}
	end.


app_bridge(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_bridge -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	Res = send_msg_locked(Node, UUID, bridge, Data),
	lager:debug("~p:handle_event -> send_msg(bridge) Res: ~p", [?MODULE, Res]),
	wait_for_complete(State),
	{next, State}.


app_sleep(Attrs, Data, #ssaml{state = Channel} = State) ->
	lager:debug("~p:app_sleep -> Attrs: ~p, Data: ~p", [?MODULE, Attrs, Data]),	
	#telephony_channel{node_fs = Node, uuid = UUID} = Channel,
	Res = send_msg(Node, UUID, sleep, Data),
	lager:debug("~p:handle_event -> send_msg(sleep) Res: ~p", [?MODULE, Res]),
	{next, State}.


-define(MAX_WAIT_EVENT, 60*1000).
wait_events(Events, State) ->
	lager:debug("~p:wait_events -> wait ~p", [?MODULE, Events]),
	wait_events(Events, State, ?MAX_WAIT_EVENT).

wait_events(Events, State, Timeout) when is_binary(Events) ->
	wait_events([Events], State, Timeout);
wait_events([], State, _) ->
	{error, State};
wait_events(Events, State, Timeout) ->
	receive
		{call_event, Data} ->
			{event, [UUID | Rest]} = Data,
			Event = proplists:get_value(<<"Event-Name">>, Rest),
			lager:debug("~p:wait_event -> ~p, ~p", [?MODULE, UUID, Event]),
			case lists:member(Event, Events) of
				true ->
					lager:debug("~p:wait_events -> ok, ~p", [?MODULE, Event]),
	 				{ok, {Event, Rest, State}};
				false ->
					wait_events(Events, State, Timeout)
			end;
		{call_hangup, _Data} ->
			lager:debug("~p:wait_events -> hangup", [?MODULE]),
			{hangup, State}
	after
		Timeout ->
			lager:debug("~p:wait_events -> timeout", [?MODULE]),
			{timeout, State}
	end.

wait_for_complete(State) ->
	lager:debug("~p:wait_for_complete -> wait..", [?MODULE]),
	{Result, Rest} = wait_events([<<"CHANNEL_EXECUTE_COMPLETE">>,
								  <<"CHANNEL_HANGUP">>,
								  <<"CHANNEL_HANGUP_COMPLETE">>], State),
	lager:debug("~p:wait_for_complete -> ~p", [?MODULE, Result]),
	{Result, Rest}.


send_msg(Node, UUID, Cmd) ->
	send_msg(Node, UUID, Cmd, "").

send_msg(Node, UUID, Cmd, Arg) ->
	ssam_telephony_dialplan:send_msg(Node, UUID, Cmd, Arg).

send_msg_locked(Node, UUID, Cmd, Arg) ->
	ssam_telephony_dialplan:send_msg_locked(Node, UUID, Cmd, Arg).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



-endif.

