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

-module(ssam_httpc).

-export([
		 encoded/1,
		 decoded/1,
		 request/2
		]).

-include_lib("ssam/include/ssam.hrl").

request(get, Url) when is_binary(Url) ->
	request(get, {Url});
request(get, {Url}) ->
	request(get, {Url, [], [], []});
request(get, {Url, Params}) ->
	request(get, {Url, Params, [], []});
request(get, {Url, Params, Headers}) ->
	request(get, {Url, Params, Headers, []});
request(get, {Url, Params, Headers, Options}) ->
	EncodedParam = param_encoded(Params),
	EncodedUrl = <<Url/bits, $?, EncodedParam/bits>>,
	lager:debug("Url: ~p", [EncodedUrl]),
	case httpc:request(get, {binary_to_list(EncodedUrl), Headers}, Options, []) of
		{ok, {{_Version, StatusCode, _ReasonPhrase}, ResHeaders, ResBody}} ->
			{ok, StatusCode, ResHeaders, ResBody};
		{?error, Reason} ->
			lager:warning("Url: ~p, Params: ~p", [Url, Params]),
			{?error, Reason}
	end;

request(post, {<<"application/x-www-form-urlencoded">> = ContentType,
			   Url, Params}) when is_list(Params) ->
	EncodedParam = param_encoded(Params),
	request(post, {ContentType, Url, EncodedParam, [], []});
request(post, {ContentType, Url, EncodedBody, Headers}) ->
	request(post, {ContentType, Url, EncodedBody, Headers, []});
request(post, {<<"application/x-www-form-urlencoded">> = ContentType,
			   Url, Params, Headers, Options}) ->
	EncodedParam = param_encoded(Params),
	request(post, {ContentType, Url, EncodedParam, Headers, Options});
request(post, {ContentType, Url, Body, Headers, Options}) ->
	case httpc:request(post, {binary_to_list(Url), Headers,
							  binary_to_list(ContentType), Body}, Options, []) of
		{ok, {{_Version, StatusCode, _ReasonPhrase}, ResHeaders, ResBody}} ->
			{ok, StatusCode, ResHeaders, ResBody};
		{?error, Reason} ->
			lager:warning("post, ~p, Url: ~p", [Reason, Url]),
			{?error, Reason}
	end.

encoded(Url) when is_binary(Url) -> cow_qs:urlencode(Url);
encoded(Url) when is_list(Url) -> http_uri:encode(Url).

decoded(Url) when is_binary(Url) -> cow_qs:urldecode(Url);
decoded(Url) when is_list(Url) -> http_uri:decode(Url).


%% Private functions

param_encoded(Params) when is_list(Params) ->
	<< <<(encoded(K))/bits, $=, (encoded(V))/bits, $&>> || {K, V} <- Params >>.
