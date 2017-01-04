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

-module(ssam_account_user_resource).

%-compile(export_all).
-export([get/2,
		 post/2,
		 put/2]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_account.hrl").


%% Public functions

get(#request{
	   uri = [<<"users">>, UserId]
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_account:get_user(UserId) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

get(#request{
	   uri = [<<"accounts">>, Sid]
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	case ssam_account:get_account(Sid) of
		{ok, Doc} ->
			{ok, ssam_rest:response(Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end;

get(#request{
	   uri = [<<"users">>, UserId, <<"authority">>]
	  } = Request, Sender) ->
	lager:info("~p:get -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	Sid = proplists:get_value(<<"sid">>, Request#request.params),
	case ssam_account:get_users_authoirty(UserId, Sid) of
		{ok, Doc} ->
			{ok, ssam_rest:response_type(?html, Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.

post(#request{
		uri = [<<"keys">>],
		account_sid = Sid, body = Props
	   } = Request, Sender) ->
	lager:info("~p:post -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	TTL = case proplists:get_value(<<"ttl">>, Props) of
		?undefined -> ?AUTH_KEY_MAX_AGE;
		Val ->
			case catch binary_to_integer(Val) of
				Num when is_number(Num) -> Num;
				_ -> ?AUTH_KEY_MAX_AGE
			end
	end,
	case ssam_account:post_key(Sid, TTL) of
		{ok, Key} ->
			Doc = {[
					{<<"key">>, Key},
					{<<"path">>, <<"/">>},
					{<<"secure">>, true},
					{<<"max_age">>, TTL}
				   ]},
			Resp = ssam_rest:response(?STATUS_CREATED, Doc, Request),
			%Resp1 = Resp#response{
					  %cookies = [{<<"key">>, Key,
								  %[{path, "/"}, {secure, true}, {max_age, TTL}
								  %]}]
					 %},
			{ok, Resp};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end;


post(#request{
		uri = [<<"users">>],
		body = Props
	   } = Request, Sender) ->
	lager:info("~p:post -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	UserId = proplists:get_value(<<"id">>, Props),
	case ssam_account:post_user(UserId, Props) of
		{ok, _} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end;


post(#request{
		uri = [<<"users">>, UserId, <<"authority">>],
		body = Props
	   } = Request, Sender) ->
	lager:info("~p:post -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	Sid = proplists:get_value(<<"sid">>, Request#request.params),
	Name = proplists:get_value(<<"name">>, Props),
	Password = proplists:get_value(<<"password">>, Props),
	case ssam_account:post_users_authority(UserId, Name, Password, Sid) of
		{ok, Doc} ->
			{ok, ssam_rest:response(?STATUS_CREATED, Doc, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end.

put(#request{
		uri = [<<"users">>, UserId],
		body = Props
	   } = Request, Sender) ->
	lager:info("~p:put -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	UserPw = proplists:get_value(<<"password">>, Props),
	Props1 = proplists:delete(<<"password">>, Props),
	case ssam_account:put_user(UserId, UserPw, Props1) of
		{ok, _} ->
			{ok, ssam_rest:response(Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_NOT_FOUND, Error, Request)}
	end.




%% Private functions



