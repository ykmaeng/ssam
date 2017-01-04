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

-module(ssam_account).

-compile(export_all).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_account.hrl").


init() ->
	ok = ssam_riakc:set_bucket(?BUCKET_ACCOUNT_KEY, [{backend, ?BACKEND_BITCASK_MONTH}]),
	lager:info("~p:init -> ok", [?MODULE]),
	ok.

configs() ->
	File = code:priv_dir(ssam_account) ++ "/conf/ssam_account.conf",
	case file:consult(File) of
		{ok, Props} -> Props;
		{error, _} -> []
	end.

routes() ->
	case proplists:get_value(routes, configs()) of
		?undefined -> [];
		Routes -> Routes
	end.

get_user(UserId) ->
	Uri = [{<<"users">>, UserId}],
	ssam_resource:get(user_idx(UserId), ?SERVICE, Uri).

post_user(UserId, Props) ->
	Uri = [{<<"users">>, []}],
	PostUser = fun() ->
		Props1 = ssam_objects:value_replaced(<<"status">>, <<"pending">>, Props),
		Props2 = ssam_objects:value_replaced(
				   <<"account_sid">>, Sid = new_account_sid(), Props1),
		case ssam_resource:post(user_idx(UserId), ?SERVICE, Uri, {Props2}) of
			{ok, Doc} ->
				case post_user_meta(Uri, UserId) of
					{ok, _} ->
						case send_mail(UserId, Sid) of
							ok -> {ok, Doc};
							Error -> Error
						end;
					Error ->
						lager:error("~p:post_user -> ~p", [?MODULE, Error]),
						delete_user(UserId), Error
				end;
			Error ->
				Error
		end
	end,
	case ssam_resource:get(user_idx(UserId), ?SERVICE, [{<<"users">>, UserId}]) of
		{ok, Doc} ->
			case ssam_objects:value(<<"status">>, Doc) of
				<<"pending">> ->
					case delete_user(UserId) of
						{ok, _} -> PostUser();
						Error -> Error
					end;
				_ ->
					{?error, ?user_already_exists}
			end;
		_ ->
			PostUser()
	end.

delete_user(UserId) ->
	Uri = [{<<"users">>, UserId}],
	case ssam_resource:delete(user_idx(UserId), ?SERVICE, Uri) of
		{ok, _} ->
			ssam_resource:delete(user_meta_key(UserId), ?SERVICE, Uri);
		Error ->
			lager:error("~p:delete_user -> ~p", [?MODULE, Error]),
			Error
	end.

post_user_meta(Uri, UserId) ->
	Props = [{<<"id">>, UserId},
			 {<<"created">>, ssam_util:now_sec()},
			 {<<"updated">>, ssam_util:now_sec()}],
	ssam_resource:post(user_meta_key(UserId), ?SERVICE, Uri, {Props}).

post_users_authority(UserId, Name, Password, Sid) ->
	post_users_authority(check, {UserId, Name, Password, Sid}).

post_users_authority(check, {UserId, Name, Password, Sid}) ->
	Uri = [{<<"users">>, UserId}],
	case ssam_resource:get(user_idx(UserId), ?SERVICE, Uri) of
		{ok, Doc} ->
			case ssam_objects:value(<<"account_sid">>, Doc) of
				Sid ->
					case ssam_objects:value(<<"status">>, Doc) of
						<<"pending">> ->
							post_users_authority(
							  update_user, {Doc, UserId, Name, Password, Sid});
						_ ->
							{?error, ?invalid_status}
					end;
				_ ->
					{?error, ?invalid_information}
			end;
		_ ->
			{?error, ?invalid_information}
	end;
post_users_authority(update_user, {Doc, UserId, Name, Password, Sid}) ->
	Uri = [{<<"users">>, UserId}],
	Doc1 = ssam_objects:value_replaced(<<"name">>, Name, Doc),
	Doc2 = ssam_objects:value_replaced(<<"status">>, <<"active">>, Doc1),
	case ssam_resource:put(user_idx(UserId), ?SERVICE, Uri, Doc2) of
		{ok, _} ->
			case put_user_meta(Uri, UserId, Password) of
				{ok, _} ->
					post_users_authority(post_account, {Doc, UserId, Password, Sid});
				Error ->
					ssam_resource:put(user_idx(UserId), ?SERVICE, Uri, Doc),
					Error
			end;
		Error ->
			Error
	end;
post_users_authority(post_account, {Doc, UserId, Password, Sid}) ->
	Token = new_token(Password),
	case post_account(UserId, Sid, secure_token(Token)) of
		{ok, _} ->
			Output = {[{<<"account_sid">>, Sid}, {<<"auth_token">>, Token}]},
			{ok, Output};
		Error ->
			Uri = [{<<"users">>, UserId}],
			ssam_resource:put(user_idx(UserId), ?SERVICE, Uri, Doc),
			Error
	end.

put_user(UserId, UserPw, Props) ->
	Uri = [{<<"users">>, UserId}],
	case ssam_resource:get(user_idx(UserId), ?SERVICE, Uri) of
		{ok, Doc} ->
			Status = ssam_objects:value(<<"status">>, Doc, <<>>),
			Sid = ssam_objects:value(<<"account_sid">>, Doc, <<>>),
			Props1 = ssam_objects:value_replaced(<<"id">>, UserId, Props),
			Props2 = ssam_objects:value_replaced(<<"status">>, Status, Props1),
			Props3 = ssam_objects:value_replaced(<<"account_sid">>, Sid, Props2),
			case ssam_resource:put(user_idx(UserId), ?SERVICE, Uri, {Props3}) of
				{ok, _} -> put_user_meta(Uri, UserId, UserPw);
				Error -> Error
			end;
		Error ->
			Error
	end.

put_user_meta(Uri, UserId, UserPw) ->
	case ssam_resource:get(user_meta_key(UserId), ?SERVICE, Uri) of
		{ok, Doc} ->
			Props = [{<<"id">>, UserId},
					 {<<"password">>, secure_password(UserId, UserPw)},
					 {<<"created">>, ssam_objects:value(
									   <<"created">>, Doc, ssam_util:now_sec())},
					 {<<"updated">>, ssam_util:now_sec()}],
			ssam_resource:put(user_meta_key(UserId), ?SERVICE, Uri, {Props});
		Error ->
			Error
	end.

get_account(Sid) ->
	Uri = [{<<"accounts">>, Sid}],
	ssam_resource:get(Sid, ?SERVICE, Uri).

post_account(UserId, Sid, Token) ->
	Uri = [{<<"accounts">>, []}],
	Props = [{<<"id">>, Sid},
			 {<<"user_id">>, UserId},
			 {<<"token">>, Token},
			 {<<"cidrs">>, []}],
	ssam_resource:post(Sid, ?SERVICE, Uri, {Props}).

get_users_authoirty(check, {UserId, Sid}) ->
	Uri = [{<<"users">>, UserId}],
	case ssam_resource:get(user_idx(UserId), ?SERVICE, Uri) of
		{ok, Doc} ->
			case ssam_objects:value(<<"account_sid">>, Doc) of
				Sid ->
					case ssam_objects:value(<<"status">>, Doc) of
						<<"pending">> ->
							get_users_authoirty(render,[{email, UserId}]);
						_ ->
							{?error, ?invalid_status}
					end;
				_ ->
					{?error, ?invalid_information}
			end;
		_ ->
			{?error, ?invalid_information}
	end;
get_users_authoirty(render, Props) ->
	case catch new_authority:module_inf() of
		{'EXIT', {undef, _}} ->
			case erlydtl:compile_file(code:priv_dir(ssam) ++
									  "/templates/new_authority.html",
									  new_authority) of
				{ok, Module} ->
					Module:render(Props);
				_ ->
					{?error, ?page_not_found}
			end;
		_ ->
			new_authority:render(Props)
	end;
get_users_authoirty(UserId, Sid) ->
	get_users_authoirty(check, {UserId, Sid}).

user_idx(UserId) ->
	<<(erlang:phash2(UserId) rem 1024):16>>.

user_meta_key(UserId) ->
	<<(user_idx(UserId))/binary, 1>>.

get_key(Id) ->
	case ssam_riakc:get(?BUCKET_ACCOUNT_KEY, Id) of
		{ok, Bin} when is_binary(Bin) ->
			{ok, binary_to_term(Bin)};
		{?error, Reason} ->
			{?error, Reason}
	end.

post_key(Sid, TTL) ->
	Key = ssam_auth:new_key(),
	case store_key(Sid, Key, TTL) of
		ok -> {ok, ssam_auth:encoded_key(Key)};
		Error -> Error
	end.

%% Private functions

store_key(Sid, {Id, Nonce}, TTL) ->
	Doc = [{sid, Sid}, {nonce, Nonce}, {ttl, TTL}, {updated, ssam_util:now_sec()}],
	ssam_riakc:put(?BUCKET_ACCOUNT_KEY, Id, Doc).

update_key(Id, Doc) ->
	ssam_riakc:put(?BUCKET_ACCOUNT_KEY, Id, Doc).

new_account_sid() ->
	ssam_util:unique_id(<<$a>>).

new_token(UserPw) ->
	ssam_util:sha(<<(crypto:hash(sha, UserPw))/binary,
					":", (ssam_util:now_mic()):56>>).

secure_password(UserId, UserPw) ->
	crypto:hash(sha, <<UserId/bits, ":", UserPw/bits>>).

secure_token(Token) ->
	crypto:hash(sha, Token).

send_mail(To, Sid)->
	TemplatePath = ssam_config:value(?SERVICE, [mail, welcome, template]),
	TemplatePath1 = erlang:iolist_to_binary([code:priv_dir(ssam), $/, TemplatePath]),
	case file:read_file(TemplatePath1) of
		{ok, Template} -> 
			Profile = ssam_config:value(?SERVICE, [mail, welcome, profile]),
			From = ssam_config:value(?SERVICE, [mail, welcome, from]),
			Subject = ssam_config:value(?SERVICE, [mail, welcome, subject]),
			Body = ssam_util:template_compiled(Template, [{"to", To}, {"sid", Sid}]),
			ssam_mailc:send(Profile, From, [{To, To}], Subject, Body);
		{?error, Reason} ->
			lager:error("~p:send_mail -> ~p", [?MODULE, Reason]),
			{?error, Reason}
	end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.

