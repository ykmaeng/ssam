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

-module(ssam_telephony_call_resource).

%-compile(export_all).
-export([post/2]).


-include_lib("ssam/include/ssam.hrl").
-include("ssam_telephony.hrl").


%% @todo Reduce the logic of state functions

%% Public functions

post(#request{
		uri = [<<"calls">>],
		id = ReqId, account_sid = Sid, body = Params
	   } = Request, Sender) ->
	lager:info("~p:post -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	From = proplists:get_value(<<"from">>, Params),
	To = proplists:get_value(<<"to">>, Params),
	App = proplists:get_value(<<"app">>, Params),
	Vars = [{K, V} || {<<"var_", K/bits>>, V} <- Params],
	Params1 = ssam_objects:list_deleted([<<"app">>], Params), 
	Params2 = ssam_objects:value_replaced(<<"acc_id">>, Sid, Params1), 
	Params3 = ssam_objects:value_replaced(<<"app_id">>, ReqId, Params2), 
	handle_post_call({From, To, App, Vars}, Request#request{body = Params3}).

%% Private functions

handle_post_call({From, To, <<"http", _/bits>> = App, Vars}, Request) ->
	ssam_telephony_fs:call(From, To, {url, App}, Vars, Request),
	{ok, ssam_rest:response(?STATUS_CREATED, Request)};

handle_post_call({From, To, <<"<tellet>", _/bits>> = App, Vars}, Request) ->
	Result = ssam_telephony_fs:call(From, To, {xml, App}, Vars, Request),
	{ok, ssam_rest:response(?STATUS_CREATED, Request)};

handle_post_call({From, To, <<"bridge", _/bits>> = App, Vars}, Request) ->
	ssam_telephony_fs:call(From, To, {app, App}, Vars, Request),
	{ok, ssam_rest:response(?STATUS_CREATED, Request)};

handle_post_call({From, To, App, Vars}, Request) ->
	case ssam_telephony_fs:call(From, To, {id, App}, Vars, Request) of
		ok ->
			{ok, ssam_rest:response(?STATUS_CREATED, Request)};
		{?error, Error} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Error, Request)}
	end.
