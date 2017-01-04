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

-module(ssam_message_topic_handler).

-behaviour(cowboy_loop_handler).
-behaviour(cowboy_websocket_handler).

%% Callbacks for Loop Handler
-export([init/3,
		 info/3,
		 terminate/3]).

%% Callbacks for REST
-export([rest_init/2,
		 is_authorized/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 allowed_methods/2,
		 malformed_request/2,
		 handle/2,
		 options/2,
		 get_resource/2,
		 post_resource/2,
		 put_resource/2,
		 delete_resource/2
		]).

%% Callbacks for Websocket
-export([websocket_init/3,
		 websocket_handle/3,
		 websocket_info/3,
		 websocket_terminate/3
		]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").


init(_Transport, Req, []) ->
	%lager:debug("~p:init -> Req: ~p", [?MODULE, Req]),
	{Method, _} = cowboy_req:method(Req),
	{Path, _} = cowboy_req:path(Req),
	{Upgrade, _} = cowboy_req:header(<<"upgrade">>, Req),
	[<<>>, _Service, _Version | Uri] = binary:split(Path, <<"/">>, [global]),
	case Uri of
		[<<"topics">>, _, <<"updates">>] when Upgrade =:= <<"websocket">> ->
			{upgrade, protocol, cowboy_websocket};
		[<<"topics">>, _, <<"updates">>] when Method =:= <<"GET">> ->
			{ok, Req1, State} = ssam_rest:parse_request(Req),
			handle_get(Req1, State);
		_ ->
			{upgrade, protocol, cowboy_rest}
	end.


%% Loop handlers

handle(Req, State) ->
	{ok, Req, State}.

info({push, TopicKey, Msgs, Ref, From}, Req, State) ->
	lager:debug("updated, TopicKey: ~p, From: ~p", [TopicKey, From]),
	case ssam_objects:value(topic_key, State#request.props) of
		TopicKey ->
			From ! {ok, Ref},
			%Msgs = binary_to_term(MsgsBin),
			Body = [{[{<<"_id">>, X#topic_message.msg_id}] ++ X#topic_message.msg_body}
					|| X <- Msgs],
			Response = ssam_rest:response(Body, State),
			info({reply, Response}, Req, State);
		_ ->
			%From ! {{?error, ?invalid_client}, Ref, self()},
			{loop, Req, State, hibernate}
	end;
info({reply, Response}, Req, State) ->
	case State#request.transport of
		eventsource ->
			ok = ssam_rest:chunk(Response, Req),
			%{loop, Req, State, ?LOOP_TIMEOUT};
			{loop, Req, State};
		_ ->
			{ok, _Req1, _State1} = ssam_rest:reply(Response, Req, State)
	end;
info({stop, TopicKey}, Req, State) ->
	lager:debug("stop, TopicKey: ~p", [TopicKey]),
	case proplists:get_value(topic_key, State#request.props) of
		TopicKey -> {ok, Req, State};
		_ -> {loop, Req, State, hibernate}
	end;
info(Unknown, Req, State) ->
	lager:warning("Unknown: ~p", [?MODULE, Unknown]),
	{loop, Req, State, hibernate}.

terminate(Reason, _Req, #request{props = Props, from = ClientPid}) ->
	lager:debug("Reason: ~p", [Reason]),
	case proplists:get_value(topic_key, Props) of
		?undefined -> lager:warning("topic_key undefined");
		TopicKey -> ssam_message_topic_server:delete_client(TopicKey, ClientPid)
	end.

handle_get(is_authorized, {Req, State}) ->
	case is_authorized(Req, State) of
		{true, Req1, State1} ->
			handle_get(check_malformed_request, {Req1, State1});
		_ ->
			handle_get(?error, {?STATUS_UNAUTHORIZED, [], Req, State})
	end;
handle_get(check_malformed_request, {Req, State}) ->
	case malformed_request(Req, State) of
		{false, _, _} ->
			handle_get(check_transport, {Req, State});
		{true, _, _} ->
			handle_get(?error, {?STATUS_BAD_REQUEST, [], Req, State})
	end;
handle_get(check_transport, {Req, State}) ->
	case cowboy_req:header(<<"accept">>, Req) of
		{<<"text/event-stream">>, Req1} ->
			State1 = State#request{transport = eventsource},
			{ok, Req2, State2} = ssam_rest:chunked_reply(Req1, State1),
			handle_get(route, {Req2, State2});
		_ ->
			handle_get(route, {Req, State})
	end;
handle_get(route, {Req, State}) ->
	case ssam_vnode:local_route(ssam_message_topic_resource, get, State#request{}) of
		{ok, State1} ->
			case
				case proplists:get_value(<<"timeout">>, State#request.params) of
					?undefined -> ?TOPIC_HTTPLOOP_TIMEOUT;
					Val -> binary_to_integer(Val) * 1000
				end
			of
				Timeout when Timeout < (30*1000) ->
					{loop, Req, State1, Timeout};
				Timeout ->
					{loop, Req, State1, Timeout, hibernate}
			end;
		{?error, Response} ->
			lager:error("Response: ~p", [Response]),
			{ok, _Req1, _State1} = ssam_rest:reply(Response, Req, State)
	end;
handle_get(?error, {Code, Msg, Req, State}) ->
	Response = ssam_rest:response(Code, Msg, State),
	{ok, _Req1, _State1} = ssam_rest:reply(Response, Req, State);
handle_get(Req, State) ->
	handle_get(is_authorized, {Req, State}).


%% REST handlers

rest_init(Req, _Options) ->
	{ok, _Req, _State} = ssam_rest:parse_request(Req).

is_authorized(Req, State) ->
	ssam_auth:is_authorized(Req, State).

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.
	
content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, get_resource}
	], Req, State}.

content_types_accepted(Req, #request{method = Method} = State) ->
	Handler = case Method of
		<<"PUT">> -> put_resource;
		<<"POST">> -> post_resource;
		Else -> throw({error, {unkown_method, Else}})
	end,
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, Handler}
	], Req, State}.

malformed_request(Req, State) ->
	{malformed_request(State), Req, State}.

malformed_request(#request{method = <<"POST">>, uri = Uri, body = Params}) ->
	case Uri of
		[<<"topics">>] -> 
			not ssam_objects:is_all_defined([<<"id">>], Params);
		[<<"topics">>, _, <<"updates">>] -> false;
		[<<"topics">>, _, <<"subscribers">>] ->
			not (
			  ssam_objects:is_all_defined([<<"id">>, <<"type">>], Params) and
			  is_valid_params(Params)
			);
		_ -> true
	end;
malformed_request(#request{method = <<"PUT">>, uri = Uri, body = Params}) ->
	case Uri of
		[<<"topics">>, _] -> false;
		[<<"topics">>, _, <<"subscribers">>, _] ->
			  not ssam_objects:is_all_defined([<<"callback_url">>], Params);
		_ -> true
	end;
malformed_request(_) ->
	false.

options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
									  <<"GET, POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
									  State#request.origin, Req1),
    %Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
    {ok, Req2, State}.

get_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_topic_resource,
									   get, State#request{}),
	lager:debug("Response: ~p", [Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

post_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_topic_resource,
											 post, State#request{}),

	lager:debug("Response: ~p", [Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

put_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_topic_resource,
									   put, State#request{}),
	lager:debug("Response: ~p", [Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

delete_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_topic_resource,
									   delete, State#request{}),
	lager:debug("Response: ~p", [Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.


%% Websocket handlers

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req1, State} = ssam_rest:parse_request(Req),
	%erlang:start_timer(10*1000, self(), <<"Hello!">>),
	case handle_get(Req1, State) of
		{ok, State1} ->
			Timeout =
				case proplists:get_value(<<"timeout">>, State#request.params) of
					?undefined -> ?TOPIC_WEBSOCKET_TIMEOUT;
					Val -> binary_to_integer(Val) * 1000
				end,
			{ok, Req1, State1, Timeout};
		{?error, Response} ->
			lager:error("Response: ~p", [Response]),
			{ok, _Req1, _State1} = ssam_rest:reply(Response, Req, State)
	end.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, _Msg}, Req, State) ->
	%erlang:start_timer(10*1000, self(), <<"How' you doin'?">>),
	%{reply, {text, Msg}, Req, State};
	{ok, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.


%% Private functions

is_valid_params(Params) ->
	case proplists:get_value(<<"type">>, Params, []) of
		<<"webhook">> ->
			ssam_objects:is_all_defined([<<"callback_url">>], Params);
		_ ->
			false
	end.

