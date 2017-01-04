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

-module(ssam_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("ssam.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:ensure_started(crypto),
	application:ensure_started(ssl),
	application:ensure_started(inets),
	application:ensure_started(ranch),
	application:ensure_started(cowboy),

    case ssam_sup:start_link() of
        {ok, Pid} ->
			ok = setup_buckets(),
			ok = load_config(),
			ok = start_sup_childs(),
			ok = start_rest_server(),
			ok = start_services(),
			ok = ssam_vnode:service_up(),
			{ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

load_config() ->
	ssam_conf:set(?PROJECT, ssam:configs()).

start_rest_server() ->
	{ok, Port} = application:get_env(ssam, api_port),
	ok = ssam_cowboy:start_protocol(https, Port, []),
	ok = ssam_cowboy:delete_routes(),
	ok = ssam_cowboy:add_routes(ssam:routes()),
	lager:info("ok"),
	ok.

start_sup_childs() ->
	Childs = [
		{ssam_mailc,
			{ssam_mailc, start_link, [_Args = []]},
			permanent, 5000, worker, [ssam_mailc]
		}
	],
	[{ok, _} = ssam_sup:add_child(Child) || Child <- Childs],
	lager:info("ok"),
	ok.

start_services() ->
	ok = ssam:start_service(ssam_monitor),
	ok = ssam:start_service(ssam_account),
	ok = ssam:start_service(ssam_message),
	ok = ssam:start_service(ssam_telephony),
	ok = ssam:start_service(ssam_storage),
	lager:info("ok"),
	ok.

setup_buckets() ->
	ok = ssam_riakc:set_bucket(?BUCKET_CACHE, [{backend, ?BACKEND_MEMORY_DAY}]),
	lager:info("ok"),
	ok.
