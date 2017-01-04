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

-module(ssam_auth).

-export([new_key/0,
		 new_key_id/0,
		 new_encoded_key/0,
		 encoded_key/1,
		 is_authorized/2]).

-include("ssam.hrl").

%% Public functions


new_key() ->
	Id = new_key_id(),
	Nonce = new_key_nonce(),
	{Id, Nonce}.

new_encoded_key() ->
	encoded_key(new_key()).

new_key_id() ->
	ssam_util:unique_bin().

is_authorized(Req, State) ->
	is_authorized({key, Req, State}).

			
%% Private functions

is_authorized({key, Req, State}) ->
	#request{params = Params, cookies = Cookies} = State,
	case proplists:get_value(<<"key">>, Cookies,
							 proplists:get_value(<<"key">>, Params, <<>>)) of
		<<>> ->
			is_authorized({basic, Req, State});
		Key ->
			case check_key(Key) of
				{?error, Reason} ->
					lager:warning("key, ~p", [Reason]),
					Req1 = cowboy_req:set_resp_cookie(
							 <<"key">>, <<>>, [{max_age, 0}, {path, "/"}, {secure, true}], Req),
					is_authorized({basic, Req1, State});
				{ok, Sid} ->
					State1 = State#request{account_sid = Sid},
					{true, Req, State1};
				{ok, Sid, NewKey} ->
					lager:debug("true"),
					Req1 = cowboy_req:set_resp_cookie(
							 <<"key">>, NewKey, [{path, "/"}, {secure, true}], Req),
					State1 = State#request{account_sid = Sid},
					{true, Req1, State1}
			end
	end;
is_authorized({basic, Req, State}) ->
	#request{account_sid = Sid, auth_token = Token} = State,
	case check_basic(Sid, Token) of
		{?error, Reason} ->
			lager:warning("basic, ~p", [Reason]),
			{{false, <<"Basic realm=api.tellet.io">>}, Req, State};
		ok ->
			lager:debug("basic, true"),
			{true, Req, State}
	end.

check_key(Key) when is_binary(Key) ->
	check_key(decode, Key).

check_key(decode, Key) ->
	case decoded_key(Key) of
		{?error, Reason} -> {?error, Reason};
		{Id, Nonce} -> check_key('query', {Id, Nonce})
	end;
check_key('query', {Id, Nonce}) ->
	case ssam_account:get_key(Id) of
		{ok, Doc} ->
			check_key(check_nonce, {Id, Nonce, Doc});
		Error ->
			Error
	end;
check_key(check_nonce, {Id, Nonce, Doc}) ->
	case proplists:get_value(nonce, Doc) of
		Nonce ->
			check_key(check_expiry, {Id, Doc});
		_ ->
			{?error, ?nonce_not_matched}
	end;
check_key(check_expiry, {Id, Doc}) ->
	case proplists:get_value(updated, Doc) of
		?undefined ->
			{?error, ?invalid_key_updated};
		Updated ->
			TTL = proplists:get_value(ttl, Doc, 0),
			case ssam_util:is_expired(Updated, TTL) of
				false ->
					%check_key(update_key, {Id, Doc});
					check_key(not_update_key, {Id, Doc});
				true ->
					{?error, ?key_expired}
			end
	end;
check_key(not_update_key, {_Id, Doc}) ->
	Sid = ssam_objects:value(sid, Doc),
	{ok, Sid};
check_key(update_key, {Id, Doc}) ->
	NewNonce = new_key_nonce(),
	Sid = ssam_objects:value(sid, Doc),
	Doc1 = ssam_objects:value_replaced(nonce, NewNonce, Doc),
	Doc2 = ssam_objects:value_replaced(updated, ssam_util:now_sec(), Doc1),
	case ssam_account:update_key(Id, Doc2) of
		ok ->
			{ok, Sid, encoded_key({Id, NewNonce})};
		Error ->
			Error
	end.


check_basic(Sid, Token) when size(Sid) > 0, size(Token) > 0 ->
	SecureToken = ssam_account:secure_token(Token),
	case ssam_account:get_account(Sid) of
		{ok, Doc} ->
			case ssam_objects:value(<<"token">>, Doc) of
				SecureToken -> ok;
				_ -> {?error, ?token_not_matched}
			end;
		{?error, {Reason, _, _}} ->
			{?error, Reason}
	end;
check_basic(_, _) ->
	{?error, ?invalid_data}.


new_key_nonce() ->
	crypto:rand_bytes(16).

encoded_key({Id, Nonce}) ->
	ssam_util:intbin2hex(<<Id/binary, Nonce/binary>>).

decoded_key(Encoded) ->
	case ssam_util:hex2intbin(Encoded) of
		Decoded when size(Decoded) =:= 32 ->
			<<Id:16/binary, Nonce:16/binary>> = Decoded,
			{Id, Nonce};
		_ ->
			{?error, ?invalid_key_encoded}
	end.


%% Tests
%%
-ifdef(TEST_).
-include_lib("eunit/include/eunit.hrl").


-endif.

