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

-module(ssam_data).

-include_lib("ssam/include/ssam.hrl").

-define(DEFAULT_TTL_SEC, 180).

%% APIs
-export([
		 value/2, value/3,
		 put/3, put/4,
		 erase/2,
		 reloaded/2, reloaded/3
		]).


put(Bucket, Key, Val) ->
	put(Bucket, Key, Val, default_props()).

put(Bucket, Key, Val, Props) ->
	ok = memoize(Bucket, Key, Val, Props),
	ok = persist(Bucket, Key, {Val, Props}).

value(Bucket, Key) ->
	value(Bucket, Key, ?undefined).

value(Bucket, Key, Default) ->
	case memoized(Bucket, Key) of
		?undefined ->
			case persisted(Bucket, Key) of
				?undefined ->
					Default;
				{Val, Props} ->
					ok = memoize(Bucket, Key, Val, Props),
					Val	
			end;
		Val ->
			Val
	end.

erase(Bucket, Key) ->
	ok = ssam_cache:delete(Bucket, Key),
	ok = ssam_riakc:delete(Bucket, Key).
	

reloaded(Bucket, Key) ->
	reloaded(Bucket, Key, ?undefined).

reloaded(Bucket, Key, Default) ->
	case persisted(Bucket, Key) of
		?undefined ->
			Default;
		{Val, Props} ->
			ok = memoize(Bucket, Key, Val, Props),
			Val	
	end.


%% Private functions

default_props() ->
	[{ttl, ?DEFAULT_TTL_SEC}].

memoize(Bucket, Key, Val, Props) ->
	ssam_cache:put(Bucket, Key, Val, Props).

persist(Bucket, Key, ValProp) ->
	ssam_riakc:put(Bucket, Key, ValProp).

memoized(Bucket, Key) ->
	ssam_cache:value(Bucket, Key, ?undefined).

persisted(Bucket, Key) ->
	case ssam_riakc:get(Bucket, Key) of
		{ok, Bin} when is_binary(Bin) ->
			{_Val, _Props} = binary_to_term(Bin);
		{ok, {Val, Props}} ->
			{Val, Props};
		{?error, _Reason} ->
			?undefined	
	end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


setup_test_() ->
	{setup,
		fun() -> 
			{ok, _} = ssam_cache:start_link()
		end,	%% initialize
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.

tests(_X) ->
	[
		{"test",
			fun() ->
				Bucket = <<"bucket">>,
				Key = term_to_binary({key, <<"test">>}),
				Val = test,
				ok = put(Bucket, Key, Val),
				Val = value(Bucket, Key)
			end
		}
	].



-endif.
