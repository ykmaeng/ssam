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

-module(ssam_lazy_worker).

-behaviour(gen_server).

-include_lib("ssam/include/ssam.hrl").

%% APIs
-export([
		 start_link/0,
		 stop/1,
		 add/1, add/2
		]).

%% Callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
         code_change/3]).

-record(state, {}).

%% Public functions

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Reason) ->
	gen_server:cast(?MODULE, {stop, Reason}).

add(Fun) -> add(Fun, []).
add(Fun, Props) ->
	gen_server:cast(?MODULE, {add, Fun, Props}).
	
%% Callback functions

init([]) ->
	{ok, #state{}}.

handle_call(Unknown, From, State) ->
	lager:warning("Unknown: ~p, From ~p", [Unknown, From]),
    {reply, ok, State}.

handle_cast({add, Fun, Props}, State) ->
	lager:debug("Fun: ~p, Props: ~p", [Fun, Props]),
    {noreply, State};

handle_cast({stop, Reason}, State) ->
	{stop, Reason, State};

handle_cast(Unknown, State) ->
	lager:error("Unknown: ~p", [Unknown]),	
    {noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:debug("~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup,
		fun() -> ok end,	%% initialize
		fun(_X) -> ok end,	%% cleanup
		fun(_X) -> [		%% tests
			tests(_X)
		] end
	}.

tests(_X) ->
	[
		{"test",
			fun() ->
				?undef
			end
		}
	].

-endif.





