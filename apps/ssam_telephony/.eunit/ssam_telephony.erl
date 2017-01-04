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

-module(ssam_telephony).

-compile(export_all).
%-export([secure_password/3]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").


%% Callback functions

configs() ->
	File = code:priv_dir(ssam_telephony) ++ "/conf/ssam_telephony.conf",
	case file:consult(File) of
		{ok, Props} -> Props;
		{error, _} -> []
	end.

routes() ->
	case proplists:get_value(routes, configs()) of
		?undefined -> [];
		Routes -> Routes
	end.

%% Public functions

secure_password(UserId, Domain, Pw) when is_list(UserId),
										 is_list(Domain),
										 is_list(Pw) ->
	ssam_util:md5(UserId ++ ":" ++ Domain ++ ":" ++ Pw);
secure_password(UserId, Domain, Pw) when is_binary(UserId),
										 is_binary(Domain),
										 is_binary(Pw) ->
	ssam_util:md5(<<UserId/bitstring, ":", Domain/bitstring, ":", Pw/bitstring>>).


%% GET domain resources

get_account_sid(DomainId) ->
	Uri = [{<<"domains">>, DomainId}],
	case ssam_resource:get(DomainId, ?SERVICE, Uri) of
		{ok, DomainGlobal} ->
			{ok, ssam_objects:value(<<"account_sid">>, DomainGlobal, <<>>)};
		Error ->
			Error
	end.

get_global_domain(DomainId) ->
	Uri = [{<<"domains">>, DomainId}],
	case ssam_resource:get(DomainId, ?SERVICE, Uri) of
		{ok, [Domain]} -> {ok, Domain};
		{ok, Domain} -> {ok, Domain};
		Error -> Error
	end.

get_domains(AccountSid) ->
	Uri = [{<<"domains">>, []}],
	case ssam_resource:get(AccountSid, ?SERVICE, Uri) of
		{ok, Doc} ->
			{ok, Doc};
		{?error, {?invalid_collection_name, _, _}} ->
			{ok, []};
		{?error, Error} ->
			{?error, Error}
	end.

get_domain(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).

get_domain(DomainId) ->
	Uri = [{<<"domains">>, DomainId}],
	case get_account_sid(DomainId) of
		{ok, AccountSid} ->
			ssam_resource:get(AccountSid, ?SERVICE, Uri);
		Error ->
			Error
	end.

get_domains_users(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, []}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).

get_domains_users(DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, []}],
	case get_account_sid(DomainId) of
		{ok, AccountSid} ->
			ssam_resource:get(AccountSid, ?SERVICE, Uri);
		Error ->
			Error
	end.

get_domains_user(AccountSid, DomainId, UserId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, UserId}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).

get_domains_user(DomainId, UserId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, UserId}],
	case get_account_sid(DomainId) of
		{ok, AccountSid} ->
			ssam_resource:get(AccountSid, ?SERVICE, Uri);
		Error ->
			Error
	end.

get_domain_user(DomainId, UserId) ->
	case get_domain(DomainId) of
		{ok, Domain} ->
			Users = ssam_objects:value(<<"users">>, Domain, []),
			User = ssam_objects:object(<<"id">>, UserId, Users),
			{ok, Domain, User};
		Error ->
			Error
	end.

get_domains_gateways(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, []}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).

get_domains_gateways(DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, []}],
	case get_account_sid(DomainId) of
		{ok, AccountSid} ->
			ssam_resource:get(AccountSid, ?SERVICE, Uri);
		Error ->
			Error
	end.

get_domains_gateway(AccountSid, DomainId, GatewayId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, GatewayId}],
	ssam_resource:get(AccountSid, ?SERVICE, Uri).

get_domains_gateway(DomainId, GatewayId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, GatewayId}],
	case get_account_sid(DomainId) of
		{ok, AccountSid} ->
			ssam_resource:get(AccountSid, ?SERVICE, Uri);
		Error ->
			Error
	end.

get_domain_gateway(DomainId, GatewayId) ->
	case get_domain(DomainId) of
		{ok, Domain} ->
			Gateways = ssam_objects:value(<<"gateways">>, Domain, []),
			GW = ssam_objects:object(<<"id">>, GatewayId, Gateways),
			{ok, Domain, GW};
		Error ->
			Error
	end.

%% POST domain resources

post_global_domain(AccountSid, DomainId) ->
	Props = [{<<"id">>, DomainId},
			 {<<"account_sid">>, AccountSid}],
	ssam_resource:post(DomainId, ?SERVICE, [{<<"domains">>, []}], {Props}).


post_domain(AccountSid, Props) ->
	DomainId = proplists:get_value(<<"id">>, Props),
	case post_global_domain(AccountSid, DomainId) of
		{ok, _} ->
			Props1 = ssam_objects:list_deleted(
					   [<<"dial-string">>, <<"users">>, <<"groups">>, <<"gateways">>],
					   Props),		
			Props2 = Props1 ++ [{<<"users">>, []},
								{<<"groups">>, []},
								{<<"gateways">>, []}],
			ssam_resource:post(AccountSid, ?SERVICE, [{<<"domains">>, []}], {Props2});
		{?error, Error} ->
			{ok, _} = delete_domain(DomainId),
			{?error, Error}
	end.

post_domains_user(AccountSid, DomainId, Props) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, []}],
	ssam_resource:post(AccountSid, ?SERVICE, Uri, {Props}).

post_domains_gateway(AccountSid, DomainId, Props) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, []}],
	ssam_resource:post(AccountSid, ?SERVICE, Uri, {Props}).

%% PUT domain resources

put_domain(AccountSid, DomainId, Props) ->
	Uri = [{<<"domains">>, DomainId}],
	case ssam_resource:get(AccountSid, ?SERVICE, Uri) of
		{ok, Domain} ->
			Users = ssam_objects:value(<<"users">>, Domain, []),
			Groups = ssam_objects:value(<<"groups">>, Domain, []),
			Gateways = ssam_objects:value(<<"gateways">>, Domain, []),
			Props1 = Props ++ [{<<"users">>, Users},
							   {<<"groups">>, Groups},
							   {<<"gateways">>, Gateways}],
			ssam_resource:put(AccountSid, ?SERVICE, Uri, {Props1});
		Error ->
			Error
	end.

put_domains_user(AccountSid, DomainId, UserId, Props) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, UserId}],
	ssam_resource:put(AccountSid, ?SERVICE, Uri, {Props}).

put_domains_gateway(AccountSid, DomainId, GatewayId, Props) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, GatewayId}],
	ssam_resource:put(AccountSid, ?SERVICE, Uri, {Props}).

%% DELETE domain resources

delete_global_domain(DomainId) ->
	ssam_resource:delete(DomainId, ?SERVICE, [{<<"domains">>, []}]).

delete_domains(AccountSid) ->
	ssam_resource:delete(AccountSid, ?SERVICE, [{<<"domains">>, []}]).

delete_domain(DomainId) ->
	ssam_resource:delete(DomainId, ?SERVICE, [{<<"domains">>, []}]).

delete_domain(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}],
	case get_domain(AccountSid, DomainId) of
		{ok, Domain} ->
			ssam_resource:put(AccountSid, ?SERVICE, Uri, Domain);
		Error ->
			Error
	end.

delete_domains_users(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, []}],
	ssam_resource:delete(AccountSid, ?SERVICE, Uri).

delete_domains_user(AccountSid, DomainId, UserId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"users">>, UserId}],
	ssam_resource:delete(AccountSid, ?SERVICE, Uri).

delete_domains_gateways(AccountSid, DomainId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, []}],
	ssam_resource:delete(AccountSid, ?SERVICE, Uri).

delete_domains_gateway(AccountSid, DomainId, GatewayId) ->
	Uri = [{<<"domains">>, DomainId}, {<<"gateways">>, GatewayId}],
	ssam_resource:delete(AccountSid, ?SERVICE, Uri).

%% Private functions


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_account_sid_test_() ->
	[
	 {"ok",
	  fun() ->
			  ok
	  end
	 }
	].


-endif.


