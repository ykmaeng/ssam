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

-module(ssam_mailc).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").

%% Publics
-export([start_link/1,
		 send/5]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).


-record(state, {
		profiles = [] :: list(tuple(atom(), record()))
	}).

-define(invalid_smtp_profile, invalid_smtp_profile).

%% Public functions

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


send(Profile, From, To, Subject, Body) ->
	gen_server:call(?MODULE, {send, Profile, From, To, Subject, Body}).

%% Callback functions

init(_Args) ->
	%process_flag(trap_exit, true),
	case ssam_conf:value(<<"ssam">>, [mailc, profiles]) of
		?undefined ->
			{ok, #state{}};
		Profiles ->
			Profiles1 = converted_profiles(Profiles),
			{ok, #state{profiles = Profiles1}}
	end.

handle_call({send, ProfileName, From, To, Subject, Body}, _From, State)
  when is_atom(ProfileName) ->
	case proplists:get_value(ProfileName, State#state.profiles) of
		?undefined ->
			{reply, {?error, ?mail_profile_not_found}, State};
		Props ->
			Type = proplists:get_value(<<"type">>, Props),
			Result = send(Type, Props, {From, To, Subject, Body}),
			{reply, Result, State}
	end;
handle_call({send, ProfileProps, From, To, Subject, Body}, _From, State)
  when is_list(ProfileProps) ->
	Type = proplists:get_value(<<"type">>, ProfileProps),
	Result = send(Type, ProfileProps, {From, To, Subject, Body}),
	{reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:warning("Reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Private functions

converted_profiles(Profiles) ->
	converted_profiles(Profiles, []).

converted_profiles([], Outputs) ->
	Outputs;
converted_profiles([Profile | Rest], Outputs) ->
	{Name, Props} = Profile,
	Props1 = case proplists:get_value(type, Props) of
		"smtp" ->
			[{<<"type">>, <<"smtp">>},
			 {<<"relay">>, list_to_binary(proplists:get_value(relay, Props, ""))},
			 {<<"port">>, proplists:get_value(port, Props, 25)}, 
			 {<<"ssl">>, proplists:get_value(ssl, Props, false)},
			 {<<"username">>, list_to_binary(proplists:get_value(username, Props, ""))},
			 {<<"password">>, list_to_binary(proplists:get_value(password, Props, ""))}];
		"http" ->
			[{<<"type">>, <<"http">>},
			 {<<"url">>, list_to_binary(proplists:get_value(url, Props, ""))},
			 {<<"method">>, list_to_binary(proplists:get_value(method, Props, ""))}];
		_ ->
			[]
	end,
	converted_profiles(Rest, Outputs ++ [{Name, Props1}]).

send(<<"smtp">>, Props, {From, To, Subject, Body}) ->
	{FromName, FromAddr} = From,
	ToAddrs = [Addr || {_, Addr} <- To],
	ToField = lists:foldl(
				fun({Name, Addr}, Bin) ->
					erlang:iolist_to_binary([Bin, Name, <<" <">>, Addr, <<">; ">>])
				end, <<>>, To),
	Content = erlang:iolist_to_binary([
			<<"Subject: ">>, Subject, <<"\r\n">>,
			<<"From: ">>, FromName, <<" <">>, FromAddr, <<"> \r\n">>,
			<<"To: ">>, ToField, " \r\n\r\n", Body
		]),
	lager:debug("smtp, Content: ~p", [Content]),

	Relay = proplists:get_value(<<"relay">>, Props),
	Port = proplists:get_value(<<"port">>, Props, 25),
	SSL = proplists:get_value(<<"ssl">>, Props, false),
	Username = proplists:get_value(<<"username">>, Props),
	Password = proplists:get_value(<<"password">>, Props),

	Result = gen_smtp_client:send({FromAddr, ToAddrs, Content},
								  [{relay, Relay}, {port, Port}, {ssl, SSL},
								   {username, Username}, {password, Password}]),
	Result.
	%gen_smtp_client:send({<<"noreply@tellet.io">>, [<<"ykmaeng@lstro.co.kr">>, <<"ykmaeng@112.219.64.70">>], <<"Subject: Welcome to Tellet\r\nFrom: Tellet <noreply@tellet.io> \r\nTo: Lstro Maeng <ykmaeng@lstro.co.kr>; YoungKook Maeng <winfavor@gmail.com> \r\n\r\nThis is the email body">>}, [{relay, <<"smtp.gmail.com">>}, {port, 465}, {ssl, true}, {username, <<"noreply.tellet@gmail.com">>}, {password, <<"pleasedon'tchange!">>}]).	


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() -> ok end,		%% init 
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.

tests(_X) ->
	[
		{"whatever",
			fun() ->
				undef
			end
		}
	].

-endif.

