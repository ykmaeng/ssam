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

-module(ssam_config).

-export([set/2,
		 value/2]).

-include_lib("ssam/include/ssam.hrl").

set(_, []) -> ok;
set(Service, Configs) when is_binary(Service) ->
	ok = delete(Service),
	ok = persist(Service, Configs),
	ok = cache(Service, Configs);
set(Service, Configs) when is_atom(Service) ->
	set(atom_to_binary(Service, latin1), Configs);
set(Service, Configs) when is_list(Service) ->
	set(list_to_binary(Service), Configs).
	

value(Service, Key) when is_binary(Service), not is_list(Key) ->
	value(Service, [Key]);
value(Service, [Key | Rest]) when is_binary(Service) -> 
	case cached(Service, Key) of
		?undefined ->
			lager:debug("cached(): undefined, Service: ~p, Key: ~p", [Service, Key]),
			?undefined;
		Value ->
			value_found(Rest, Value)
	end;
value(Service, Key) when is_atom(Service) ->
	value(atom_to_binary(Service, latin1), Key);
value(Service, Key) when is_list(Service) ->
	value(list_to_binary(Service), Key).

value_found([], Value) -> Value;
value_found([Key | Rest], Props) ->
	case proplists:get_value(Key, Props) of
		?undefined -> ?undefined;
		Value -> value_found(Rest, Value)
	end.


cache(_, []) -> ok;
cache(Service, [{K, V} | Rest]) ->
	ssam_cache:put(Service, conf_key(K), V),
	cache(Service, Rest);
cache(Service, [Invalid | Rest]) ->
	lager:error("invalid format!, Service: ~p, Data: ~p", [Service, Invalid]),
	cache(Service, Rest).

cached(Service, Key) ->
	ssam_cache:value(Service, conf_key(Key)).

persist(Service, Props) ->
	ok = ssam_riakc:put(<<"ssam_conf">>, Service, Props).


delete(Service) ->
	ssam_cache:delete(Service).

conf_key(Key) -> {conf, Key}.



%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.


