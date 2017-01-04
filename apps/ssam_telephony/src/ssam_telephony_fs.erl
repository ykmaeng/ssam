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

-module(ssam_telephony_fs).

-behaviour(gen_server).

-export([
		start_link/0,
		bind/1,
		subscribe_events/1,
		register_event_handler/0,
		bgapi/2,
		call/2, call/3, call/5
	]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").

-define(XML_DOCTYPE, <<"freeswitch/xml">>).
-define(XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-record(state, {
		  node_fs :: ?undefined | atom(),
		  node_erlang :: ?undefined | binary()
		 }).

%% API
start_link() ->
	lager:debug("~p:start_link -> called", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

bind(Type) ->
	gen_server:call(?MODULE, {bind, Type}).

register_event_handler() ->
	gen_server:call(?MODULE, {register_event_handler}).

subscribe_events(Events) ->
	gen_server:call(?MODULE, {event, Events}).

bgapi(Command, Args) ->
	gen_server:call(?MODULE, {bgapi, Command, Args}).

call(Url, AppId) -> call(Url, AppId, []).
call(Url, AppId, Options) ->
	gen_server:call(?MODULE, {call, Url, AppId, Options}).

call(From, To, App, Vars, Request) ->
	gen_server:call(?MODULE, {call, From, To, App, Vars, Request}).



%% Callback functions
init([]) ->
	lager:debug("~p:init -> called", [?MODULE]),
	%process_flag(trap_exit, true),
	CheckFs = fun(NodeName) ->
		spawn_link(
		  fun() -> timer:sleep(1000), check_freeswitch(NodeName) end
		), ok
	end,
	NodeFs = ssam_config:value(?SERVICE, [freeswitch, node_fs]),
	NodeErlang = ssam_config:value(?SERVICE, [freeswitch, node_erlang]),
	State = #state{node_fs = list_to_atom(NodeFs),
				   node_erlang = list_to_binary(NodeErlang)},
	Result = CheckFs(NodeFs),
	{Result, State}.

check_freeswitch(Node) when is_list(Node) ->
	check_freeswitch(list_to_atom(Node));
check_freeswitch(Node) when is_atom(Node) ->
	lager:debug("~p:check_freeswitch -> Node: ~p", [?MODULE, Node]),
	case net_adm:ping(Node) of
		pong ->
			connect();
		Else ->
			io:format("~n~nElse: ~p~n~n", [Else]),
			timer:sleep(2*1000),
			check_freeswitch(Node)
	end.

connect() ->
	lager:debug("~p:connect -> called", [?MODULE]),
	%bind(configuration),
	bind(directory),
	bind(dialplan),
	subscribe_events([
					  'CHANNEL_CALLSTATE'
					 ]),
	register_event_handler().


handle_call({bind, Type}, _From, State) ->
	Node = State#state.node_fs,
	lager:debug("~p:handler_call({bind, ~p}) -> Node: ~p", [?MODULE, Type, Node]),
	{fs, Node} ! {bind, Type},
    {reply, ok, State};
handle_call({event, Events}, _From, State) ->
	Node = State#state.node_fs,
	lager:debug("~p:handler_call({event, ~p}) -> Node: ~p", [?MODULE, Events, Node]),
	{fs, Node} ! list_to_tuple([event] ++ Events),
    {reply, ok, State};
handle_call({register_event_handler}, _From, State) ->
	Node = State#state.node_fs,
	lager:debug("~p:handler_call({register_event_handler}) -> Node: ~p", [?MODULE, Node]),
	{fs, Node} ! register_event_handler,
    {reply, ok, State};
handle_call({bgapi, Command, Args}, _From, State) ->
	Node = State#state.node_fs,
	Result = freeswitch:bgapi(Node, Command, Args),
	lager:debug("~p:handle_call -> bgapi, Result: ~p", [?MODULE, Result]),
    {reply, ok, State};

handle_call({call, From, To, {xml, App}, Vars, Request}, _From, State) ->
	lager:debug("~p:handler_call -> call, ~p, ~p, ~p, ~p",
				[?MODULE, From, To, App, Vars]),
	#request{account_sid = Sid, id = ReqId, body = Params} = Request,
	AppId = proplists:get_value(<<"app_id">>, Params),
	App1 = re:replace(App, <<"[$\t]">>, <<>>, [global, unicode, {return, binary}]),
	ok = ssam_riakc:put(<<?SERVICE/bits, "_calls">>,
					   <<Sid/bits, ?DELIMITER, ReqId/bits>>,
					   [{id, AppId}, {app, App1}, {vars, Vars}]),

	Node = State#state.node_fs,
	Var = case length(Params) > 0 of
		true ->	
			 case << <<$,, X/bits, $=, Y/bits>> || {X, Y} <- Params >> of
				 <<>> -> <<>>;
				 <<_:8, V/bits>> -> <<${, V/bits, $}>>
			 end;
		false ->
			<<>>
	end,
	Arg = <<Var/bits, To/bits, $\s, AppId/bits>>,
	Res = {fs, Node} ! {bgapi, originate, Arg},
	lager:debug("~p:handle_call -> originate, ~p", [?MODULE, Res]),
    {reply, ok, State};

handle_call({call, From, To, {url, URL}, Vars, Request}, _From, State) ->
	lager:debug("~p:handler_call -> call, ~p, ~p, ~p, ~p",
				[?MODULE, From, To, URL, Vars]),
	#request{account_sid = Sid, id = ReqId, body = Params} = Request,
	AppId = proplists:get_value(<<"app_id">>, Params),
	App =
		case ssam_httpc:request(get, URL) of
			{ok, _StatusCode, _ResHeaders, ResBody} -> ResBody;
			_ -> <<>>
		end,
	App1 = re:replace(App, <<"[$\t]">>, <<>>, [global, unicode, {return, binary}]),
	ok = ssam_riakc:put(<<?SERVICE/bits, "_calls">>,
					   <<Sid/bits, ?DELIMITER, ReqId/bits>>,
					   [{id, AppId}, {app, App1}, {vars, Vars}]),

	Node = State#state.node_fs,
	Var = case length(Params) > 0 of
		true ->	
			 case << <<$,, X/bits, $=, Y/bits>> || {X, Y} <- Params >> of
				 <<>> -> <<>>;
				 <<_:8, V/bits>> -> <<${, V/bits, $}>>
			 end;
		false ->
			<<>>
	end,
	Arg = <<Var/bits, To/bits, $\s, AppId/bits>>,
	Res = {fs, Node} ! {bgapi, originate, Arg},
	lager:debug("~p:handle_call -> originate, ~p", [?MODULE, Res]),
    {reply, ok, State};

handle_call({call, From, To, {app, App}, Vars, Request}, _From, State) ->
	lager:debug("~p:handler_call -> call, ~p, ~p, ~p, ~p",
				[?MODULE, From, To, App, Vars]),
	#request{account_sid = Sid, id = ReqId, body = Params} = Request,
	Node = State#state.node_fs,
	Var = case length(Params) > 0 of
		true ->	
			 case << <<$,, X/bits, $=, Y/bits>> || {X, Y} <- Params >> of
				 <<>> -> <<>>;
				 <<_:8, V/bits>> -> <<${, V/bits, $}>>
			 end;
		false ->
			<<>>
	end,
	Arg = <<Var/bits, To/bits, $\s, $&, App/bits>>,
	Res = {fs, Node} ! {bgapi, originate, Arg},
	lager:debug("~p:handle_call -> originate, ~p", [?MODULE, Res]),
    {reply, ok, State};



handle_call({call, Url, AppId, Options}, _From, State) ->
	lager:debug("~p:handler_call -> call, ~p, ~p, ~p", [?MODULE, Url, AppId, Options]),
	Node = State#state.node_fs,
	Var = case length(Options) > 0 of
		true ->	
			 case << <<$,, X/bits, $=, Y/bits>> || {X, Y} <- Options >> of
				 <<>> -> <<>>;
				 <<_:8, V/bits>> -> <<${, V/bits, $}>>
			 end;
		false ->
			<<>>
	end,
	Arg = <<Var/bits, Url/bits, $\s, AppId/bits>>,
	Res = {fs, Node} ! {bgapi, originate, Arg},
	lager:debug("~p:handle_call -> originate, ~p", [?MODULE, Res]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_event({<<"Event-Name">>, <<"CHANNEL_BRIDGE">> = Event},
			 UniqueId, Params, _State) ->
	lager:debug("~p:handle_event(~p) -> UniqueId: ~p, Params: ~p",
				[?MODULE, Event, UniqueId, Params]),
	%FromHost = proplists:get_value(<<"variable_sip_from_host">>, Params, <<>>),
	%FromUser = proplists:get_value(<<"variable_sip_from_user">>, Params, <<>>),
	%AccountSid = get_account_sid(FromHost),
	ok;
handle_event({<<"Event-Name">>, <<"CHANNEL_UNBRIDGE">> = Event},
			 UniqueId, Params, _State) ->
	io:format("~p:handle_event(~p) -> UniqueId: ~p~n", [?MODULE, Event, UniqueId]),
	ok;
handle_event(Event, UniqueId, Params, _State) ->
	io:format("~p:handle_event(~p) -> UniqueId: ~p~n", [?MODULE, Event, UniqueId]),
	ok.

%% Messages for FreeSwitch EVENT "CHANNEL_BRIDGE"
handle_info({event, [UniqueId, Event | Params]}, State) ->
	handle_event(Event, UniqueId, Params, State),
    {noreply, State};

%% Messages for FreeSwitch FETCH "DIRECTORY"
handle_info({fetch, directory, Tag, Key, Value, FetchId, Params}, State) ->
	%io:format("~p:handle_info({fetch, directory, ~p, ~p, ~p, ~p}) ~p~n~p~n",
			  %[?MODULE, Tag, Key, Value, FetchId, self(), Params]),
	Node = State#state.node_fs,
	Purpose = proplists:get_value(<<"purpose">>, Params, <<>>),
	Action = proplists:get_value(<<"action">>, Params, <<>>),
	case fetch_directory({Purpose, Action, Params}, State) of
		{ok, Xml, State1} ->
			{fs, Node} ! {fetch_reply, FetchId, Xml},
			{noreply, State1};
		{error, Msg, State1} ->
			lager:error("~p:handle_info -> error, Msg: ~p", [?MODULE, Msg]),
			{noreply, State1}
	end;

%% Messages for FreeSwitch FETCH "DIALPLAN"
handle_info({fetch, dialplan, _Tag, _Key, _Value, FetchId, Params}, State) ->
	%io:format("~p:handle_info({fetch, dialplan, ~p}) ~p~n~p~n",
			  %[?MODULE, FetchId, self(), Params]),
	Node = State#state.node_fs,
	CallDirection = proplists:get_value(<<"Call-Direction">>, Params),
	CallState = proplists:get_value(<<"Channel-Call-State">>, Params),
	case fetch_dialplan({CallDirection, CallState, Params}, State) of
		{ok, Xml, State1} ->
			{fs, Node} ! {fetch_reply, FetchId, Xml},
			{noreply, State1};
		{error, Msg, State1} ->
			lager:error("~p:handle_info -> error, Msg: ~p", [?MODULE, Msg]),
			{noreply, State1}
	end;

handle_info({'EXIT', _Pid, Reason}, State) ->
	error_logger:info_msg("freeswitch ~p: 'EXIT' by ~p~n",[self(), Reason]),
	%check_freeswitch(State#state.node_fs),
    {noreply, State};
handle_info(Info, State) ->
	error_logger:info_msg("freeswitch ~p: ~p~n",[self(), Info]),
    {noreply, State}.

terminate(Reason, _State) ->
	error_logger:info_msg("freeswitch ~p: terminate! ~p~n",[self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions 


fetch_directory({<<>>, <<"sip_auth">>, Params}, State) ->
	{ok, Xml} = state_fetch_directory(authentication, Params),
	{ok, Xml, State};	
fetch_directory({<<>>, <<"message-count">>, Params}, State) ->
	{ok, Xml} = state_fetch_directory(authentication, Params),
	{ok, Xml, State};	
fetch_directory({<<>>, <<"user_call">>, Params}, State) ->
	{ok, Xml} = state_fetch_directory(authentication, Params),
	{ok, Xml, State};

fetch_directory({<<"gateways">>, <<>>, Params}, State) ->
	Profile = proplists:get_value(<<"profile">>, Params, <<>>),
	lager:debug("~p:fetch_directory -> gateways, ~p", [?MODULE, Profile]),
	{ok, xml_not_found(), State};
fetch_directory({<<"network-list">>, [], _Params}, State) ->
	{ok, xml_not_found(), State}.

fetch_dialplan({<<"inbound">>, <<"RINGING">>, Params}, State) ->
	{ok, Xml} = state_fetch_dialplan(inbound_ringing, Params),
	{ok, Xml, State};
fetch_dialplan({<<"outbound">>, <<"EARLY">>, Params}, State) ->
	{ok, Xml} = state_fetch_dialplan(outbound_active, {Params, State#state.node_erlang}),
	{ok, Xml, State};
fetch_dialplan({<<"outbound">>, <<"ACTIVE">>, Params}, State) ->
	{ok, Xml} = state_fetch_dialplan(outbound_active, {Params, State#state.node_erlang}),
	{ok, Xml, State}.

state_fetch_dialplan(inbound_ringing, Params) ->
	Context = proplists:get_value(<<"Caller-Context">>, Params),
	Dnis = proplists:get_value(<<"Caller-Destination-Number">>, Params),
	_Ani = proplists:get_value(<<"Caller-ANI">>, Params),
	Domain = proplists:get_value(<<"variable_domain_name">>, Params),
	_Xml1 = {document, [{type, ?XML_DOCTYPE}],
		   [{section, [{name, <<"dialplan">>}],
			 [{context, [{name, Context}],
			   [{extension,
				 [{condition, [
					   %{action, [{application, <<"answer">>}], []},
					   %{action, [{application, <<"park">>}], []}]
					   %{action, [
								 %{application, <<"playback">>},
								 %{data, <<"ivr/ivr-welcome_to_freeswitch.wav">>}
								%], []},
					   {action, [{application, <<"bridge">>},
								 {data, <<"user/", Dnis/bits, "@", Domain/bits>>}], []}
					  ]}] 
				}]
			  }]
			}]
		  },
	AccId = Domain,
	AppId = Dnis,
	URL = <<"http://localhost:3000/dialplan_", Dnis/bits, ".xml">>,
	App =
		case ssam_httpc:request(get, URL) of
			{ok, _StatusCode, _ResHeaders, ResBody} -> ResBody;
			_ -> <<>>
		end,
	App1 = re:replace(App, <<"[$\t]">>, <<>>, [global, unicode, {return, binary}]),
	ok = ssam_riakc:put(<<?SERVICE/bits, "_calls">>,
					   <<AccId/bits, ?DELIMITER, AppId/bits>>,
					   [{id, AppId}, {app, App1}, {vars, []}]),
	Xml = {document, [{type, ?XML_DOCTYPE}],
		   [{section, [{name, <<"dialplan">>}],
			 [{context, [{name, Context}],
			   [{extension,
				 [{condition, [
					   {action, [{application, <<"set">>}, {data, <<"acc_id=", AccId/bits>>}], []},
					   {action, [{application, <<"set">>}, {data, <<"app_id=", AppId/bits>>}], []},
					   {action, [{application, <<"erlang">>}, {data, <<"ssam_telephony_dialplan:launch ssam1@node1.tellet.io">>}], []}
					  ]}] 
				}]
			  }]
			}]
		  },
	lager:debug("~p:state_fetch_dialplan() -> ok, Xml: ~p", [?MODULE, Xml]),
	Xml1 = xmerl:export_simple([Xml], xmerl_xml, [{prolog, ?XML_PROLOG}]),
	Xml2 = unicode:characters_to_binary(Xml1),
	{ok, Xml2};

state_fetch_dialplan(outbound_active, {Params, ErlangNode}) ->
	Context = proplists:get_value(<<"Caller-Context">>, Params),
	Xml = {document, [{type, ?XML_DOCTYPE}],
		   [{section, [{name, <<"dialplan">>}],
			 [{context, [{name, Context}],
			   [{extension,
				 [{condition, [
					   {action, [{application, <<"erlang">>},
								 {data, <<"ssam_telephony_dialplan:launch ",
										  ErlangNode/bits>>}], []}
					  ]}] 
				}]
			  }]
			}]
		  },
	lager:debug("~p:state_fetch_dialplan() -> ok, Xml: ~p", [?MODULE, Xml]),
	Xml1 = xmerl:export_simple([Xml], xmerl_xml, [{prolog, ?XML_PROLOG}]),
	Xml2 = unicode:characters_to_binary(Xml1),
	{ok, Xml2}.

state_fetch_directory(authentication, Params) ->
	state_fetch_directory(get_domain, Params);
state_fetch_directory(get_domain, Params) ->
	DomainId = proplists:get_value(<<"domain">>, Params, []),
	UserId = proplists:get_value(<<"user">>, Params, []),
	case ssam_telephony:get_domain_user(DomainId, UserId) of
		{ok, [], _} ->
			state_fetch_directory(?error, {domain_not_found});
		{ok, _, []} ->
			state_fetch_directory(?error, {user_not_found});
		{ok, Domain, User} ->
			{DomainProps} = Domain,
			Domain1 = {DomainProps ++ default_params(domain)},
			state_fetch_directory(xml, {start, Domain1, DomainId, User, UserId});
		{?error, {Reason, _, _}} ->
			state_fetch_directory(?error, {Reason})
	end;
state_fetch_directory(xml, {start, Domain, DomainId, User, UserId}) ->
	try
		{document, [{type, ?XML_DOCTYPE}],
		 [{section, [{name, <<"directory">>}],
		  [{domain, [{name, DomainId}],
			[
			 state_fetch_directory(xml, {domain_params, Domain}),
			 state_fetch_directory(xml, {domain_variables, Domain}),
			 state_fetch_directory(xml, {users, DomainId, UserId, User}),
			 state_fetch_directory(xml, {gateways, Domain})
			]
		   }]
		  }]
		}
	of
		Xml -> state_fetch_directory(ok, {Xml})
	catch
		Type:Except ->
			lager:error("~p:state_fetch_directory(xml, ..) -> ~p:~p",
						[?MODULE, Type, Except]),
			state_fetch_directory(error, {xml_build_failed})
	end;
state_fetch_directory(xml, {domain_params, Domain}) ->
	{Props} = Domain,
	Props1 = ssam_objects:list_deleted(
			   [<<"id">>, <<"account_sid">>, <<"users">>,
				<<"groups">>, <<"gateways">>],
			   Props),
	{params, [{param, [{name, K}, {value, V}], []}
			  || {K, V} <- Props1, not is_variable(K)]};
state_fetch_directory(xml, {domain_variables, Domain}) ->
	{Props} = Domain,
	{variables,
	 [begin
		  <<"variable_", Name/bits>> = K,
		  {variable, [{name, Name}, {value, V}], []}
	  end || {K, V} <- Props, is_variable(K)]
	};
state_fetch_directory(xml, {users, DomainName, UserId, User}) ->
	{groups,
	 [{group, [{name, DomainName}],
	   [{users,
		 [{user, [{id, UserId}],
		   [state_fetch_directory(xml, {user_params, User}),
			state_fetch_directory(xml, {user_variables, User})]
		  }]
		}]
	  }]
	};
state_fetch_directory(xml, {gateways, Domain}) ->
	Gateways = ssam_objects:value(<<"gateways">>, Domain),
	{gateways,
	 [{gateway, [{name, ssam_objects:value(<<"id">>, GW)}],
	   [state_fetch_directory(xml, {gateway_params, GW}),
		state_fetch_directory(xml, {gateway_variables, GW})]
	  } || GW <- Gateways] 
	};
state_fetch_directory(xml, {user_params, User}) ->
	{Props} = User,
	Props1 = ssam_objects:deleted(<<"id">>, Props),
	{params, [{param, [{name, K}, {value, V}], []}
			  || {K, V} <- Props1, not is_variable(K)]};
state_fetch_directory(xml, {user_variables, User}) ->
	{Props} = User,
	{variables,
	 [begin
		  <<"variable_", Name/bits>> = K,
		  {variable, [{name, Name}, {value, V}], []}
	  end || {K, V} <- Props, is_variable(K)]
	};
state_fetch_directory(xml, {gateway_params, GW}) ->
	{Props} = ssam_objects:deleted(<<"id">>, GW),
	{params, [{param, [{name, K}, {value, V}], []}
			  || {K, V} <- Props, not is_variable(K)]};
state_fetch_directory(xml, {gateway_variables, GW}) ->
	{Props} = GW,
	{variables,
	 [begin
		  <<"variable_", Name/bits>> = K,
		  {variable, [{name, Name}, {value, V}], []}
	  end || {K, V} <- Props, is_variable(K)]
	};
state_fetch_directory(ok, {Xml}) ->
	lager:debug("~p:state_fetch_directory() -> ok, Xml: ~p", [?MODULE, Xml]),
	Xml1 = xmerl:export_simple([Xml], xmerl_xml, [{prolog, ?XML_PROLOG}]),
	Xml2 = unicode:characters_to_binary(Xml1),
	{ok, Xml2};
state_fetch_directory(?error, {Msg}) ->
	lager:error("~p:state_fetch_directory() -> error, Msg: ~p", [?MODULE, Msg]),
	{ok, xml_not_found()}.


xml_not_found() ->
	Xml = {document, [{type, <<"freeswitch/xml">>}],
		   [{section, [{name, <<"result">>}],
			 [{result, [{status, <<"not found">>}], []}]
			}]
		  },
	Xml1 = xmerl:export_simple([Xml], xmerl_xml, [{prolog, ?XML_PROLOG}]),
	Xml2 = unicode:characters_to_binary(Xml1),
	Xml2.


is_variable(<<"variable_", _/bits>>) -> true;
is_variable(_) -> false.
	
default_params(domain) ->
	[{<<"dial-string">>, <<"{presence_id=${dialed_user}@${dialed_domain}}${sofia_contact(${dialed_user}@${dialed_domain})}">>}].


%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_variable_test() ->
	?assert(is_variable(<<"variable_a">>) =:= true),
	?assert(is_variable(<<"variable">>) =:= false).




-endif.

%{params, begin {P} = ssam_objects:value(<<"params">>, DomainObj), P end},
%{variables, begin {V} = ssam_objects:value(<<"variables">>, DomainObj), V end},
%{users, begin {P} = ssam_objects:value(<<"params">>, DomainObj), P end}

