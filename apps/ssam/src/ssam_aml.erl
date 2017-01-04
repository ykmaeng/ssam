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

-module(ssam_aml).

-export([
		 start/2,
		 tellet/3,
		 set/3,
		 block/3,
		 'case'/3,
		 'if'/3,
		 exec/3,
		 goto/3,
		 include/3,
		 loop/3,
		 context/3,
		 escape/3,
		 stop/3,

		 '_temporary'/0
		]).

-include_lib("ssam/include/ssam.hrl").

-define(APP_LOOP_MAXCOUNT, 10).

start(Xml, State) ->
	case parse(Xml) of
		{ok, Parsed} ->
			route([Parsed], [], State#ssaml{doc = Parsed});
		{?error, Reason} ->
			lager:error("~p", [Reason]),
			{?error, Reason}
	end.

parse(Xml) ->
	case erlsom:simple_form(Xml, [{nameFun, fun erlsom_callback/3}]) of
		{ok, Parsed, _Rest} ->
			{ok, Parsed};
		{?error, Reason} ->
			lager:error("~p, Xml: ~p", [Reason, Xml]),
			{?error, Reason}
	end.


tellet(_, Data, State) ->
	{enter, Data, State}.

set([], _, State) ->
	{next, State};
set(Attrs, "", State) ->
	OldVars = State#ssaml.vars,
	NewVars = ssam_util:updated_props(OldVars, Attrs),
	State1 = State#ssaml{vars = NewVars},
	{next, State1};
set(Attrs, Data, State) ->
	case proplists:get_value("var", Attrs) of
		?undef ->
			{next, State};
		Var ->
			OldVars = State#ssaml.vars,
			NewVars = ssam_util:updated_props(OldVars, [{Var, Data}]),
			State1 = State#ssaml{vars = NewVars},
			{next, State1}
	end.

block(_, Data, State) ->
	{enter, Data, State}.

'case'(Attrs, Data, State) ->
	case check_regex(Attrs, State) of
		{true, State1} -> {escape, Data, State1};
		{false, State1} -> {next, State1}
	end.

'if'(Attrs, Data, State) ->
	case check_regex(Attrs, State) of
		{true, State1} -> {enter, Data, State1};
		{false, State1} -> {next, State1}
	end.

exec([], _, State) ->
	{next, State};
exec([{"app", App} | _], _, State) ->
	Element = app_element(App, State#ssaml.doc),
	{enter, Element, State};
exec([{"fun", Fun} | _], _, State) ->
	Element = fun_element(Fun, State#ssaml.doc),
	{enter, Element, State}.

goto([], _, State) ->
	{next, State};
goto([{"app", App} | _], _, State) ->
	Element = app_element(App, State#ssaml.doc),
	{goto, Element, State};
goto([{"fun", Fun} | _], _, State) ->
	Element = fun_element(Fun, State#ssaml.doc),
	{goto, Element, State}.

include(_, _, State) ->
	{next, State}.

loop(Attrs, Data, State) ->
	CountStr = proplists:get_value("count", Attrs, "1"),
	case string:to_integer(CountStr) of
		{error, _} ->
			{next, State};
		{N, _Rest} when is_number(N), N > 1 ->
			Count = if N > ?APP_LOOP_MAXCOUNT -> ?APP_LOOP_MAXCOUNT;
					   true -> N end,
			{enter, copied(Data, Count), State};
		_ ->
			{enter, Data, State}
	end.

context(_, _, State) ->
	{next, State}.

escape(_, _, State) ->
	{escape, [], State}.

stop(_, _, State) ->
	{stop, State}.

'_temporary'() ->
	trimmed(''),
	httpc_request('','','').

%% Private functions

-define(MODULE_AML, "ssam_aml").
-define(MODULE_MESSAGE_AML, "ssam_message_aml").
-define(MODULE_TELEPHONY_AML, "ssam_telephony_aml").

module(_, "topic") -> ?MODULE_MESSAGE_AML;
module(_, "sms") -> ?MODULE_MESSAGE_AML;
module(_, "mms") -> ?MODULE_MESSAGE_AML;

module(_, "answer") -> ?MODULE_TELEPHONY_AML;
module(_, "hangup") -> ?MODULE_TELEPHONY_AML;
module(_, "say") -> ?MODULE_TELEPHONY_AML;
module(_, "play") -> ?MODULE_TELEPHONY_AML;
module(_, "input") -> ?MODULE_TELEPHONY_AML;
module(_, "bridge") -> ?MODULE_TELEPHONY_AML;
module(_, "message") -> ?MODULE_TELEPHONY_AML;
module(_, "conference") -> ?MODULE_TELEPHONY_AML;

module(_, _) -> ?MODULE_AML.

trimmed(Subject) ->
	trimmed(Subject, "^[\r\n\t\s]*|[\r\n\t\s]*$").

trimmed(Subject, Regex) ->
	re:replace(Subject, Regex, "", [global, unicode, {return, list}]).


httpc_request(get, Url, []) ->
	httpc_request(get, {Url, [], [], []}, [], []);
httpc_request(get, Url, [_Param | Rest]) ->
	httpc_request(get, Url, Rest).

httpc_request(Method,
			  {Url, Headers, ContentType, Body},
			  HttpOptions,
			  Options) ->
	case httpc:request(Method,
					   {Url, Headers, ContentType, Body},
					   HttpOptions,
					   Options) of
		{ok, Result} ->
			{ok, Result};
		{error, Reason} ->
			{error, Reason}
	end.

check_regex([], State) -> {true, State};
check_regex([{Name, Regex} | Tail], State) ->
	Vars = State#ssaml.vars,
	case proplists:get_value(Name, Vars, []) of
		[] ->
			{false, State};
		Val ->
			case re:run(Val, Regex, [unicode, {capture, [1], list}]) of
				{match, []} ->
					check_regex(Tail, State);
				{match, [Captured]} ->
					NewVars = ssam_util:updated_props(Vars, [{Name ++ "_1", Captured}]),
					State1 = State#ssaml{vars = NewVars},
					check_regex(Tail, State1);
				nomatch ->
					{false, State}
			end
	end.


app_element(_Id, _Doc) ->
	undefined.

fun_element([], _) -> [];
fun_element(Id, {{_, "tellet"}, _, Data}) ->
	fun_element(Id, Data);
fun_element(Id, [{{_, "fun"}, Attrs, Data} | Rest]) ->
	Attrs1 = [{K, V} || {{_, K}, V} <- Attrs],
	case proplists:get_value("id", Attrs1, []) of
		Id -> Data;
		_ -> fun_element(Id, Rest)
	end;
fun_element(Id, [_ | Rest]) ->
	fun_element(Id, Rest);
fun_element(_, []) -> [].

copied(_, 1, Copied) -> Copied;
copied(List, N, Copied) when N > 1 ->
	Copied1 = Copied ++ List,
	copied(List, N-1, Copied1).

copied(List, N) when is_list(List) ->
	copied(List, N, List).

execute({{Prefix, Name}, Attrs, [Data]}, State) when is_list(Data) ->
	Data1 = ssam_util:template_compiled(Data, State#ssaml.vars),
	execute({{Prefix, Name}, Attrs, Data1}, State);
execute({{Prefix, Name}, Attrs, Data}, State) ->
	Vars = State#ssaml.vars,
  	Module = list_to_atom(Prefix),
	Fun = list_to_atom(Name),
	Attrs1 = [{K, ssam_util:template_compiled(V, Vars)} || {{_, K}, V} <- Attrs],
	case catch Module:Fun(Attrs1, Data, State) of
		{'EXIT', {undef, _}} ->
			{next, State};
		{'EXIT', {Reason, Stack}} ->
			lager:error("'EXIT' {~p, ~p}", [Reason, Stack]),
			{error, Reason};
		Result ->
			Result
	end.

route([Element | Tail], Rests, State) when is_tuple(Element) ->
	case execute(Element, State) of
		{enter, Childs, State1} ->
			route(Childs, [Tail | Rests], State1);
		{next, State1} ->
			route(Tail, Rests, State1);
		{escape, Childs, State1} ->
			route(Childs, Rests, State1);
		{goto, Childs, State1} ->
			route(Childs, [], State1);
		{stop, State1} ->
			{ok, stoped, State1};
		{error, Error} ->
			{error, Error}
	end;
route([], [], State) ->
	{ok, finished, State};
route([], Rests, State) ->
	[H | T] = Rests,
	route(H, T, State);
route([_ | []], _, State) ->
	{error, invalid_element, State}.


erlsom_callback(Name, _Namespace, Prefix) ->
	case module(Prefix, Name) of
		error -> {[], Name};
		Val -> {Val, Name}
	end.

any() ->
	fun list:any/2.

-ifdef(TEST_).
-include_lib("eunit/include/eunit.hrl").


xml_depth() ->
	<<"<tellet>
		<if>
			<if>
				<say>1</say>
				<if>
					<say>2</say>
					<if>
						<say>3</say>
					</if>
					<if>
						<escape/>
						<say>4</say>
					</if>
				</if>
				<if>
					<say>5</say>
				</if>
			</if>
			<if>
				<say>6</say>
			</if>
		</if>
		<if>
			<say>7</say>
		</if>
    </tellet>">>.

xml_set() ->
	<<"<tellet>
		<say>안녕하세요?</say>
		<set name=\"맹영국\" age=\"36\"/>
		<say>{{name}}: 내 이름은 {{name}}, 나이는 {{age}}세 입니다.</say>
		<set code=\"\">
			<![CDATA[
				function matchwo(a,b)
				{
					if (a < b && a < 0) then {
						return 1;
					}
					else {
					  return 0;
					}
				}
			]]>
		</set>
    </tellet>">>.

xml_if() ->
	<<"<tellet>
		<set name=\"Kook\"/>
		<if name=\"^[kK]ook$\">
			<say>I'm {{name}}!</say>
			<say>Good bye!</say>
			<stop/>
		</if>
		<say>I'm not Kook!</say>
    </tellet>">>.

xml_loop() ->
	<<"<tellet>
		<loop count=\"2\">
			<say>1</say>
			<loop count=\"1\">
				<say>2</say>
			</loop>
			<loop>  <!-- same as continue -->
				<say>3</say>
			</loop>
			<loop count=\"wrong\"> <!-- ignored -->
				<say>4</say>
			</loop>
		</loop>
		<say>Good bye!</say>
    </tellet>">>.

xml_block() ->
	<<"<tellet>
		<set name=\"kook\"/>
		<block>
			<case name=\"^[kK]ook\">
				<say>I'm Kook!</say>
			</case>
			<case>
				<say>IGNORED!</say>
			</case>
		</block>
    </tellet>">>.

xml_goto_local() ->
	<<"<tellet>
		<context name=\"1\">
			<say>context 1 - start</say>
			<goto context=\"2\" return=\"yes\"/>
			<say>context 1 - end</say>
		</context>

		<say>start</say>
		<goto context=\"1\" return=\"yes\"/>

		<context name=\"2\">
			<say>context 2</say>
		</context>

		<say>end</say>
    </tellet>">>.

xml_goto_remote() ->
	<<"<tellet>
		<goto url=\"http://example.com\" context=\"context-1\">
			<param
				from=\"kook\"
				to=\"maeng\"
				title=\"Hello, world!\"
			/>
		</goto>
    </tellet>">>.

xml_include() ->
	<<"<tellet>
		<include url=\"http://example.com\" method=\"get\">
			<param
				from=\"kook\"
				to=\"maeng\"
				title=\"Hello, world!\"
			/>
		</goto>
    </tellet>">>.


parse_test() ->
    Xmls = [
			%xml_depth(),
			%xml_set(),
			%xml_if(),
			%xml_loop(),
			%xml_switch(),
			xml_goto_local()
			%xml_goto_remote()
		   ],
	F = fun(Xml) ->
		Result = start(Xml, #ssaml{}),
		io:format(user, "~n~nResult: ~p~n", [Result])
	end,
	[F(X) || X <- Xmls].

-endif.
