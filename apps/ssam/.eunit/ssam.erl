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

-module(ssam).

-include("ssam.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
		 start_service/1,
		 stop_service/1,
		 restart_service/1,
		 reload_service/1,
		 reload_all/0,
		 configs/0,
		 routes/0,
         ping/0,
         ping/1,
         ping/2,
         ping/3
        ]).


-type account_sid() :: binary().
-type request_id() :: binary().
-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type timestamp() :: pos_integer().
-type props() :: list(tuple(atom(), term())).

-export_type([
			  account_sid/0,
			  request_id/0,
			  ip4_address/0,
			  timestamp/0,
			  props/0
			 ]).


%% Public API

start_service(Service) ->
	Module = module_name(Service),
	ok = ssam_conf:set(Module, Module:configs()),
	ok = application:start(Module),
	ok = ssam_cowboy:add_routes(Module:routes()).

stop_service(Service) ->
	Module = module_name(Service),
	%% @todo Remove the service's routes
	%% ssam_cowboy:remove_routes(Routes),
	application:stop(Module),
	application:unload(Module).

restart_service(Service) ->
	stop_service(Service),
	start_service(Service).

reload_all() ->
	reload_service("").

reload_service(Service) when is_atom(Service) ->
	reload_service(atom_to_list(Service));
reload_service("ssam_" ++ Service) ->
	reload_service(Service);
reload_service(Service) when is_list(Service) ->
	F = fun({Mod, Info}) when is_list(Info) ->
			case re:run(Info, "ssam_"++Service++"[_a-zA-Z]*\.beam") of
				{match, _} ->
					true = code:soft_purge(Mod),
					{true, code:load_file(Mod)};
				_ ->
					false
			end;
		   (_) ->
				false
		end,
	lists:filtermap(F, code:all_loaded()).

configs() ->
	File = code:priv_dir(ssam) ++ "/conf/ssam.conf",
	{ok, Props} = file:consult(File),
	Props.

routes() ->
	case proplists:get_value(routes, configs()) of
		?undefined -> [];
		Routes -> Routes
	end.
	

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, ssam),
	lager:debug("~p ping() -> get_apl(): ~p", [self(), PrefList]),
	[IndexNode] = PrefList,
    riak_core_vnode_master:sync_spawn_command(
		IndexNode, {<<"test">>, <<"ping">>}, ssam_vnode_master).


ping(Key, N) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, Key}),
    PrefList = riak_core_apl:get_apl(DocIdx, N, ssam),
	lager:debug("~p ping() -> get_apl(): ~p", [self(), PrefList]),
    riak_core_vnode_master:command(PrefList, ping, ssam_vnode_master).

ping(N) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, N, ssam),
	lager:debug("~p ping() -> get_apl(): ~p", [self(), PrefList]),
    riak_core_vnode_master:command(PrefList, ping, ssam_vnode_master).

ping(App, Msg, N) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, N, ssam),
	lager:debug("~p ping() -> get_apl(): ~p", [self(), PrefList]),
	VnodeMaster = case App of
		admin -> ssam_admin_vnode_master;
		message -> ssam_message_vnode_master;
		telephony -> ssam_telephony_vnode_master;
		storage -> ssam_storage_vnode_master;
		[] -> ssam_vnode_master
	end,
    riak_core_vnode_master:command(PrefList, Msg, VnodeMaster).



%% Private functions

module_name(Service) when is_atom(Service) ->
	module_name(atom_to_list(Service));
module_name(Service) when is_binary(Service) ->
	module_name(binary_to_list(Service));
module_name("ssam_"++Service) when is_list(Service) ->
	module_name(Service);
module_name(Service) when is_list(Service) ->
	list_to_atom("ssam_"++Service).

