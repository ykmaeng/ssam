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

-module(ssam_rest).


-export([
		 parse_request/1, parse_request/2,
		 response/1, response/2, response/3,
		 response_type/3, response_type/4,
		 body/2,
		 reply/3,
		 chunk/2,
		 chunked_reply/2
		]).

-include("ssam.hrl").

parse_request(CowboyReq) ->
	parse_request(CowboyReq, [{body, qs}]).

parse_request(CowboyReq, Options) ->
	{Path, _} = cowboy_req:path(CowboyReq),
	[<<>>, Service, Version | Uri] = binary:split(Path, <<"/">>, [global]),
	{Method, _} = cowboy_req:method(CowboyReq),
	{Params, _} = cowboy_req:qs_vals(CowboyReq),
	{ContentType, _} = cowboy_req:header(<<"content-type">>, CowboyReq),
	{Cookies, _} = cowboy_req:cookies(CowboyReq),
	{Origin, _} = cowboy_req:header(<<"origin">>, CowboyReq),
	{Referer, _} = cowboy_req:header(<<"referer">>, CowboyReq),
	[AccountSid, AuthToken] = parse_account_info(CowboyReq),
	RequestId = ssam_util:unique_id(<<"r">>),
	State = #request{
		id = RequestId,
		account_sid = AccountSid,
		auth_token = AuthToken,
		version = Version,
		service = Service,
		uri = [ssam_httpc:decoded(X) || X <- Uri, X =/= <<>>],
		path = Path,
		method = Method,
		params = Params,
		body = body(proplists:get_value(body, Options), CowboyReq),
		content_type = ContentType,
		origin = Origin,
		referer = Referer,
		cookies = Cookies,
		from = self()
	},
	{CowboyReq1, State1} = set_resp_header(CowboyReq, State),
	{ok, CowboyReq1, State1}.

set_resp_header(CowboyReq, State) ->
	CowboyReq1 = set_resp_header(request_id, CowboyReq, State#request.id),
	CowboyReq2 = set_resp_header(cors, CowboyReq1, State#request.origin),
	{CowboyReq2, State}.

set_resp_header(request_id, CowboyReq, ReqId) ->
	cowboy_req:set_resp_header(<<"ssam-request-id">>, ReqId, CowboyReq);
set_resp_header(cors, CowboyReq, Origin) when is_binary(Origin),
											  size(Origin) > 0 ->
	CowboyReq1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
											Origin, CowboyReq),
	CowboyReq2 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>,
											<<"true">>, CowboyReq1),
	CowboyReq2;
set_resp_header(_, CowboyReq, _) ->
	CowboyReq.


response(State) ->
	response(?STATUS_OK, <<>>, State).

response(Code, State) when is_number(Code) ->
	response(Code, <<>>, State);
response(Doc, State) ->
	response(?STATUS_OK, Doc, State).

response(Code, Msg, State) when is_atom(Msg) and Code < 400 ->
	response(Code, {[{<<"message">>, Msg}]}, State);
response(Code, Msg, State) when is_atom(Msg) ->
	response(Code, {[{<<"reason">>, Msg}]}, State);
response(Code, {Item, Msg}, State) ->
	response(Code, {[{Item, Msg}]}, State);
response(Code, {Reason, Coll, Id}, State) ->
	Doc = case 
		case {Coll, Id} of
			{[], []} -> <<>>;
			{_, []} -> <<Coll/bits>>;
			{[], _} -> <<Id/bits>>;
			{<<>>, <<>>} -> <<>>;
			{_, <<>>} -> <<Coll/bits>>;
			{<<>>, _} -> <<Id/bits>>;
			_ ->  <<Coll/bits, "/", Id/bits>>
		end
	of
		<<>> ->
			{[{<<"reason">>, Reason}]};
		Resource ->
			{[{<<"reason">>, Reason}, {<<"resource">>, Resource}]}
	end,
	response(Code, Doc, State);
response(Code, Doc, State) ->
	#response{account_sid = State#request.account_sid,
			  request_id = State#request.id,
			  code = Code, body = json_body(Code, Doc)}.

response_type(Type, Doc, State) ->
	response_type(Type, ?STATUS_OK, Doc, State).

response_type(Type, Code, Doc, State) ->
	#response{account_sid = State#request.account_sid,
			  request_id = State#request.id,
			  content_type = Type, code = Code, body = Doc}.

reply(#response{content_type = Type, code = Code, body = Body, cookies = Cookies},
	  CowboyReq, State) ->
	Headers = [content_type(Type) | default_headers(State)],
	CowboyReq1 = lists:foldl(fun({K, V, Options}, Req) ->
								 cowboy_req:set_resp_cookie(K, V, Options, Req)
							 end, CowboyReq, Cookies),
	{ok, CowboyReq2} = cowboy_req:reply(Code, Headers, Body, CowboyReq1),
	{ok, CowboyReq2, State}.

chunked_reply(CowboyReq, State) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>} | default_headers(State)],
	{ok, CowboyReq1} = cowboy_req:chunked_reply(200, Headers, CowboyReq),
	{ok, CowboyReq1, State}.

chunk(#response{body = Body}, CowboyReq) ->
	JsonData = jiffy:encode(Body),
	ok = cowboy_req:chunk(<<"data: ", JsonData/bits, "\n\n">>, CowboyReq).

%% Private functions

content_type(?undefined) -> content_type(?json);
content_type(?json) -> {<<"content-type">>, <<"application/json">>};
content_type(?xml) -> {<<"content-type">>, <<"application/xml">>};
content_type(?html) -> {<<"content-type">>, <<"text/html">>};
content_type(?plain) -> {<<"content-type">>, <<"text/plain">>};
content_type(?atom) -> {<<"content-type">>, <<"application/atom+xml">>};
content_type(?'form-data') -> {<<"content-type">>, <<"multipart/form-data">>};
content_type(?'x-www-form') -> {<<"content-type">>, <<"application/x-www-form-urlencoded">>};
content_type(Type) when is_binary(Type) -> {<<"content-type">>, Type};
content_type({Type, SubType, _Param}) -> {<<"content-type">>, <<Type/bits, $/, SubType/bits>>}.

parse_account_info(CowboyReq) ->
	{Auth, _} = cowboy_req:header(<<"authorization">>, CowboyReq, <<>>),
	AuthDecoded = case Auth of
		<<>> -> <<>>;
		<<"Basic ", Base64/bits>> ->
			base64:mime_decode(Base64);
		_ ->
			base64:mime_decode(Auth)
	end,
	case binary:split(AuthDecoded, <<":">>, [global]) of
		[Sid] -> [Sid, <<>>];
		[Sid, Token] -> [Sid, Token];
		_ -> [<<>>, <<>>]
	end.

body(qs, CowboyReq) ->
	case cowboy_req:body_qs(CowboyReq) of
		{ok, Props, _} -> Props;
		_ -> <<>>
	end;
body(raw, CowboyReq) ->
	case cowboy_req:body(CowboyReq) of
		{ok, Bin, _} -> Bin;
		_ -> <<>>
	end;
body(?undefined, _CowboyReq) ->
	?undefined.

default_headers(_State) ->
	[].


json_body(_, <<>>) -> <<>>;
json_body(Code, Doc) when Code < 400, is_tuple(Doc) ->
	%jiffy:encode({[{<<"resource">>, Doc}]});
	jiffy:encode(Doc);
json_body(Code, Doc) when Code < 400, is_list(Doc) ->
	%jiffy:encode({[{<<"resources">>, Doc}]});
	jiffy:encode(Doc);
json_body(Code, Doc) when Code < 400, is_binary(Doc) ->
	jiffy:encode({[{<<"message">>, Doc}]});
json_body(_, Doc) ->
	jiffy:encode({[{<<"error">>, Doc}]}).



