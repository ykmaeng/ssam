-module(ssam_storage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_storage.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = setup_buckets(),
	ssam_storage_sup:start_link().

stop(_State) ->
    ok.

setup_buckets() ->
	ok = ssam_riakc:set_bucket(?BUCKET_STORAGE_BUCKET, [{backend, ?BACKEND_ELEVELDB_STORAGE}]),
	lager:info("~p:setup_buckets -> ok", [?MODULE]).

