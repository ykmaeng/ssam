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

-module(ssam_cowboy).

-export([
		 start_protocol/3,
		 add_routes/1,
		 delete_routes/0
		]).

-include("ssam.hrl").

-define(BUCKET, <<"ssam_cowboy">>).
-define(KEY, <<"routes">>).

start_protocol(Protocol, Port, Routes) ->
	Dispatch = cowboy_router:compile(Routes),
	start(Protocol, {Port, Dispatch}).

start(http, {Port, Dispatch}) ->
	{ok, _} = cowboy:start_http(
				ssam_http, 10, [{port, Port}], [{env, [{dispatch, Dispatch}]}]
			  );
start(https, {Port, Dispatch}) ->
	{ok, _} = cowboy:start_https(
				ssam_https, 10,
				[{port, Port},
				 {certfile, code:priv_dir(ssam) ++ "/ssl/unified.crt"},
				 {keyfile, code:priv_dir(ssam) ++ "/ssl/tellet.io.key"}],
				[{env, [{dispatch, Dispatch}]}]
			  ),
	ok.

add_routes(Routes) ->
	lager:debug("Routes: ~p", [Routes]),
	case get_routes() of
		{ok, OldRoutes} ->
			{ok, NewRoutes} = reduce_routes(OldRoutes ++ Routes),
			ok = set_routes(NewRoutes),
			ok = store_routes(NewRoutes);
		{?error, Reason} ->
			{?error, Reason}
	end.


%% Private functions

get_routes() ->
	case ssam_riakc:get(?BUCKET, ?KEY) of
		{ok, Bin} -> {ok, binary_to_term(Bin)};
		{?error, Reason} -> {?error, Reason}
	end.

delete_routes() ->
	ok = ssam_riakc:put(?BUCKET, ?KEY, []).

set_routes(Routes) ->
	ok = cowboy:set_env(?ssam_https, dispatch, cowboy_router:compile(Routes)).

store_routes(Routes) ->
	ok = ssam_riakc:put(?BUCKET, ?KEY, term_to_binary(Routes)).

reduce_routes(Routes) ->
	Keys = proplists:get_keys(Routes),
	reduce_routes(Keys, Routes, []).

reduce_routes([Key|Tail], Routes, NewRoutes) ->
	Routes1 = lists:usort(proplists:append_values(Key, Routes)), %% removes duplicates
	Routes2 = proplists:delete(Key, Routes1),
	NewRoutes1 = NewRoutes ++ [{Key, Routes1}],
	reduce_routes(Tail, Routes2, NewRoutes1);
reduce_routes([], _, Routes) ->
	{ok, Routes}.



