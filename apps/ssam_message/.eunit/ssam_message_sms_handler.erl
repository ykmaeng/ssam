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

-module(ssam_message_sms_handler).

-export([init/3,
		 rest_init/2,
		 is_authorized/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 allowed_methods/2,
		 malformed_request/2,
		 delete_resource/2,
		 get_resource/2,
		 put_resource/2,
		 post_resource/2]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").


init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Options) ->
	{ok, _Req, _State} = ssam_rest:parse_request(Req).

is_authorized(Req, State) ->
	ssam_auth:is_authorized(Req, State).

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.
	
content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, get_resource}
	], Req, State}.

content_types_accepted(Req, State) ->
	Handler = case cowboy_req:method(Req) of
		{<<"PUT">>, _} -> put_resource;
		{<<"POST">>, _} -> post_resource;
		{Else, _} -> throw({error, {unkown_method, Else}})
	end,
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, Handler}
	], Req, State}.

malformed_request(Req, State) ->
	{malformed_request(State), Req, State}.

malformed_request(#request{method = <<"POST">>,
						   uri = Uri,
						   body = Body}) ->
	case Uri of
		[<<"sms">>] -> 
			not ssam_objects:is_all_defined([<<"from">>, <<"to">>, <<"message">>], Body);
		_ -> true
	end;
malformed_request(_) ->
	false.

get_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_sms_resource,
									 get, State#request{}),
	lager:debug("~p:get_resource ~p Response: ~p", [?MODULE, self(), Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

post_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_sms_resource,
									 post, State#request{}),

	lager:debug("~p:post_resource -> Response: ~p", [?MODULE, Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

put_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_sms_resource,
									 put, State#request{}),
	lager:debug("~p:put_resource ~p Response: ~p", [?MODULE, self(), Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

delete_resource(Req, State) ->
	{_, Response} = ssam_vnode:route(ssam_message_sms_resource,
									 delete, State#request{}),
	lager:debug("~p:delete_resource ~p Response: ~p", [?MODULE, self(), Response]),
	{ok, Req1, State1} = ssam_rest:reply(Response, Req, State),
	{halt, Req1, State1}.

%% Private functions
%%



