-module(ssam_message_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	TopicManager = ?CHILD(ssam_message_topic_manager, worker, []),
	MsgProviders =
		case ssam_conf:value(?SERVICE, [providers]) of
			?undefined -> [];
			Providers ->
				[begin
					 case proplists:get_value(use, Props) of
						 yes ->
							 Handler = proplists:get_value(handler, Props),
							 ?CHILD(Handler, worker, []);
						 _ -> []
					 end
				 end || {_Name, Props} <- Providers]

		end,
	Childs = [TopicManager | MsgProviders],
	lager:debug("Childs: ~p", [Childs]),
    {ok, { {one_for_one, 5, 10}, lists:flatten(Childs)} }.

