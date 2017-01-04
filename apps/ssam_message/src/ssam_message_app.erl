-module(ssam_message_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ok = setup_buckets(),
    ssam_message_sup:start_link().

stop(_State) ->
    ok.

setup_buckets() ->
	ok = ssam_riakc:set_bucket(?BUCKET_MESSAGE_LOG, [{backend, ?BACKEND_BITCASK_WEEK}]),
	ok = ssam_riakc:set_bucket(?BUCKET_MESSAGE_SMS, [{backend, ?BACKEND_BITCASK_QUARTER}]),
	ok = ssam_riakc:set_bucket(?BUCKET_MESSAGE_LMS, [{backend, ?BACKEND_BITCASK_QUARTER}]),
	lager:info("~p:setup_buckets -> ok", [?MODULE]).
