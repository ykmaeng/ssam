%% base62 encoding - initial idea based on riak's encoding, with no if's and fixed base
%% gleicon - 2010
%% 
%% testing:
%% 0> c(base62.erl). 
%% 1> base62:decode(base62:encode(1000)) == 1000.
%% charlist removed thanks to freenode's #erlang
%%
 
-module(base62).
-export([encode/1, decode/1]).
 
numbychar(I) when I >= 36 -> I-36+$a;
numbychar(I) when I >= 10 -> I-10+$a;
numbychar(I) -> I+$0.
 
nthchar(N) when N =< 9 -> $0 + N;
nthchar(N) when N =< 35 -> $A +N - 10;
nthchar(N) -> $a + N - 36.
 
encode(Id) -> encode(Id, []).
encode(Id, Acc) when Id < 0 -> encode(-Id, Acc); 
encode(Id, []) when Id =:= 0 -> "0";
encode(Id, Acc) when Id =:= 0 -> Acc;
encode(Id, Acc) ->
	R = Id rem 62,
	Id1 = Id div 62,
	Ac1 = [nthchar(R)-1|Acc],
	encode(Id1, Ac1).
 
decode(Str) ->
	decode(lists:reverse(Str), lists:flatlength(Str), 0).	
decode([], _, Acc) -> erlang:trunc(Acc);
decode(Str, Olen, Acc) ->
	[B|C] = Str,
	Pos = Acc + numbychar(B) * math:pow(62, Olen - lists:flatlength(Str)),
	decode(C, Olen, Pos).
