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

-module(ssam_util).

-export([
		 to_lower/1,
		 to_upper/1,
		 char_to_utf8/1,
		 utf8_to_bin/1,
		 hmac_sha1/2, hmac_sha1/3,
		 throw_if/2,
		 now_sec/0,
		 now_mic/0,
		 now_format/1, now_format/2,
		 sha/1,
		 unique_bin/0,
		 unique_id/0, unique_id/1,
		 unique_base64/0,
		 intbin2hex/1,
		 hex2intbin/1,
		 next_item/2,
		 keygen/1, keygen/2,
		 updated_props/2,
		 is_expired/2,
		 cidr_network_ip4/2,
		 cidr_netmask_ip4/1,
		 check_cidr/2,
		 template_compiled/2, template_compiled/3,
		 nonce/1,
		 combined/1
		]).

-include_lib("ssam/include/ssam.hrl").

%% Public functions

to_lower(Bits) ->
	<< <<(case X of X when X >= $A, X =< $Z -> X+($a-$A); X -> X end)>>
	   || <<X>> <= Bits >>.

to_upper(Bits) ->
	<< <<(case X of X when X >= $a, X =< $z -> X+($A-$a); X -> X end)>>
	   || <<X>> <= Bits >>.

char_to_utf8(Chars) ->
	unicode:characters_to_list(Chars, utf8).

utf8_to_bin(Chars) ->
	unicode:characters_to_binary(Chars).

hmac_sha1(Key, Data) ->
	crypto:hmac(sha, Key, Data).

hmac_sha1(Key, Data, base64) ->
	base64:encode(hmac_sha1(Key, Data)).

throw_if(Expr, Reason) ->
	if Expr -> throw(Reason); true -> ok end.

now_sec() ->
	{Mega, Sec, _Micro} = os:timestamp(),
	Mega * 1000000 + Sec.

now_mic() ->
	{Mega, Sec, Micro} = os:timestamp(),
	(Mega * 1000000 + Sec) * 1000000 + Micro.

now_format(Format, Values) ->
	list_to_binary(io_lib:format(Format, Values)).

now_format(yyyymmdd) ->
	{YY, MM, DD} = erlang:date(),
	now_format("~b~2..0b~2..0b", [YY, MM, DD]);
now_format(hhmmss) ->
	{HH, MM, SS} = erlang:time(),
	now_format("~2..0b~2..0b~2..0b", [HH, MM, SS]).

sha(Bin) when is_binary(Bin) ->
	BinHash = crypto:hash(sha, Bin),
	list_to_binary([io_lib:format("~2.16.0b", [C]) || <<C>> <= BinHash]);
sha(Str) when is_list(Str) ->
	BinHash = crypto:hash(sha, Str),
	lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= BinHash]).

unique_id(Prefix) when is_binary(Prefix) ->
	<<_, Id/bits>> = unique_id(),
	<<Prefix/bits, Id/bits>>.

unique_id() ->
	Bin = unique_bin(),
	%lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= Bin]). %% to string
	intbin2hex(Bin).

unique_bin() ->
	Now = <<(now_mic()):56>>,
	Node = <<(erlang:phash2(node())):32>>,
	Rand = crypto:rand_bytes(5),
	<<Now/binary, Node/binary, Rand/binary>>. %% 128bits binary

unique_base64() ->
	base64:encode(unique_bin()).

intbin2hex(Bin) ->
	list_to_binary([io_lib:format("~2.16.0b", [C]) || <<C>> <= Bin]).

hex2intbin(HexaBin) ->
	case catch <<(binary_to_integer(HexaBin, 16)):(trunc(size(HexaBin)/2*8))>> of
		{'EXIT', {badarg, _}} -> <<>>;
		Bin -> Bin
	end.
	%<< <<(binary_to_integer(X, 16))/integer>> || <<X:16/bits>> <= HexaBin >> %% too slow

next_item(_, []) -> [];
next_item(Item, [Item| []]) -> [];
next_item(Item, List) ->
	case lists:dropwhile(fun(X) -> X =/= Item end, List) of
		[_ | []] -> lists:nth(1, List);
		[_ | Tail] -> lists:nth(1, Tail);
		[] -> lists:nth(1, List)
	end.

keygen(List) ->
	keygen(List, ?DELIMITER).

keygen([H | T], Delimiter) ->
	keygen(T, Delimiter, H).

keygen([], _, Output) -> Output;
keygen([H | T], Delimiter, Output) ->
	Output1 = <<Output/bits, Delimiter, H/bits>>,
	keygen(T, Delimiter, Output1).


updated_props(OldProps, NewProps) when is_list(OldProps),
									  is_list(NewProps) ->
	lists:foldl(
	  fun({N, _}, OldProps1) ->
			  proplists:delete(N, OldProps1)
	  end, OldProps, NewProps
	 ) ++ NewProps.

is_expired(_, ?forever) -> false;
is_expired(_, ?infinite) -> false;
is_expired(Timestamp, TTL) ->
	(Timestamp + TTL < now_sec()).

cidr_network_ip4({I1, I2, I3, I4}, Bits) when is_integer(Bits), Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,
    case (Bits div 8) of
        0 -> {(I1 band Last), 0, 0, 0};
        1 -> {I1, (I2 band Last), 0, 0};
        2 -> {I1, I2, (I3 band Last), 0};
        3 -> {I1, I2, I3, (I4 band Last)};
        4 -> {I1, I2, I3, I4}
    end.

cidr_netmask_ip4(Bits) when is_integer(Bits), Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,
    case (Bits div 8) of
        0 -> {(255 band Last), 0, 0, 0};
        1 -> {255, (255 band Last), 0, 0};
        2 -> {255, 255, (255 band Last), 0};
        3 -> {255, 255, 255, (255 band Last)};
        4 -> {255, 255, 255, 255}
    end.

check_cidr(SourceIp, {FilterIp, Bits}) ->
	cidr_network_ip4(SourceIp, Bits) =:=
	cidr_network_ip4(FilterIp, Bits).


template_compiled([], Subject, _) -> Subject;
template_compiled([[Matched] | Tail], Subject, DicProps) ->
	DestVal = proplists:get_value(Matched, DicProps, ""),
	Subject1 = re:replace(Subject,
						  "\\{\\{" ++ Matched ++ "\\}\\}",
						  DestVal,
						  [global, unicode, {return, list}]),
	template_compiled(Tail, Subject1, DicProps).

template_compiled(Subject, DicProps) ->
	Regex = "\\{\\{([^\\{\\}]+)\\}\\}",
	case re:run(Subject, Regex, [global, unicode, {capture, [1], list}]) of
		{match, MatchedL} ->
			template_compiled(MatchedL, Subject, DicProps);
		nomatch -> Subject 
	end.

nonce(Length) ->
	list_to_binary(
	  [random:uniform(94) + 32 || _ <- lists:seq(1, Length)]
	).

combined(List) ->
	combined(List, ?DELIMITER).

combined([H | T], Delimiter) ->
	combined(T, Delimiter, H).

combined([], _, Output) -> Output;
combined([H | T], Delimiter, Output) ->
	Output1 = <<Output/bits, Delimiter, H/bits>>,
	combined(T, Delimiter, Output1).



%% Private functions
%%




%% Tests
%%
-ifdef(TEST_).
-include_lib("eunit/include/eunit.hrl").

-endif.
