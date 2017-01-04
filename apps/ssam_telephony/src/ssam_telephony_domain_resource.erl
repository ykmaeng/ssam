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

-module(ssam_telephony_domain_resource).

%-compile(export_all).
-export([get/2, post/2, put/2, delete/2]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").


%% @todo Reduce the logic of state functions

%% Public functions
get(#request{
		uri = [<<"domains">>],
		account_sid = Sid
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:get_domains(Sid) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;
get(#request{
	   uri = Uri,
	   account_sid = Sid
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_resource:get(Sid, ?SERVICE, Uri) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

post(#request{
		uri = [<<"domains">>],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:post_domain(Sid, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end;
post(#request{
		uri = [<<"domains">>, DomainId, <<"users">>],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:post_domains_user(Sid, DomainId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end;
post(#request{
		uri = [<<"domains">>, DomainId, <<"gateways">>],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:post_domains_gateway(Sid, DomainId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end.

put(#request{
		uri = [<<"domains">>, DomainId],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:put_domain(Sid, DomainId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;
put(#request{
		uri = [<<"domains">>, DomainId, <<"users">>, UserId],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:put_domains_user(Sid, DomainId, UserId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;
put(#request{
		uri = [<<"domains">>, DomainId, <<"gateways">>, GatewayId],
		account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_telephony:put_domains_gateway(Sid, DomainId, GatewayId, Params) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

delete(#request{
		uri = [<<"domains">>, _],
		account_sid = Sid
	   } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_resource:get(Sid, ?SERVICE, Request#request.uri) of
		{ok, Domain} ->
			Domain1 = ssam_objects:value_replaced(<<"_request">>, <<"delete">>, Domain),
			Domain2 = ssam_objects:value_replaced(<<"_status">>, <<"pending">>, Domain1),
			case ssam_resource:put(Sid, ?SERVICE, Request#request.uri, Domain2) of
				{ok, _} ->
					{ok, ssam_rest:response(Request)};
				{?error, Error} ->
					{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
			end;
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;
delete(Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_resource:delete(Request#request.account_sid, ?SERVICE,
							  Request#request.uri) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

%% Private functions

