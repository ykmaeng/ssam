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

-module(ssam_message_lms_resource).

%-compile(export_all).
-export([post/2]).

-include_lib("ssam/include/ssam.hrl").
-include("ssam_message.hrl").



post(#request{
		uri = [<<"lms">>]
	   } = Request, Sender) ->
	lager:info("~p:post -> RequestId: ~p, Path: ~p, Sender: ~p",
			   [?MODULE, Request#request.id, Request#request.path, Sender]),
	state_post_lms(check_callback_url, Request).



%% Private functions

state_post_lms(check_callback_url, #request{body = Body} = Request) ->
	case proplists:get_value(<<"callback_url">>, Body) of
		?undefined ->
			state_post_lms(check_msg_id, Request);
		CallbackUrl ->
			Code = ssam_message:new_verification_code(),
			case ssam_message:confirm_url(
				   CallbackUrl, [{<<"verification_code">>, Code}], Code) of
				ok ->
					state_post_lms(check_msg_id, Request);
				{?error, Reason} ->
					{?error, ssam_rest:response(?STATUS_BAD_REQUEST, Reason, Request)}
			end
	end;
state_post_lms(check_msg_id, Request) ->
	#request{id = ReqId, account_sid = Sid, body = Body} = Request,
	MsgId = case proplists:get_value(<<"id">>, Body) of
				?undefined -> ReqId;
				Id -> Id
			end,
	case ssam_riakc:get(?BUCKET_MESSAGE_LMS, log_key(Sid, MsgId)) of
		{?error, ?not_found} ->
			state_post_lms(check_receivers, {MsgId, Request});
		{ok, _} ->
			{?error, ssam_rest:response(
					   ?STATUS_BAD_REQUEST, ?id_duplicated, Request)}
	end;
state_post_lms(check_receivers, {MsgId, Request}) ->
	To = proplists:get_value(<<"to">>, Request#request.body),
	To1 = re:replace(To, <<"[$\s.-]">>, <<>>, [global, unicode, {return, binary}]),
	ToList = re:split(To1, <<"[,;]">>, [unicode, {return, binary}]),
	ToList1 = lists:usort(ToList), %% No more duplicates
	case length(ToList1) > 0 of
		true ->
			state_post_lms(write_ref, {MsgId, ToList1, Request});
		false ->
			{?error, ssam_rest:response(?STATUS_BAD_REQUEST, ?no_receivers, Request)}
	end;
state_post_lms(write_ref, {MsgId, ToList, Request}) ->
	#request{id = RequestId, account_sid = Sid} = Request,
	RefId = ssam_util:unique_base64(),
	Doc = {[
			{<<"account_sid">>, Sid},
			{<<"request_id">>, RequestId},
			{<<"type">>, <<"lms">>},
			{<<"msg_id">>, MsgId},
			{<<"created">>, ssam_util:now_sec()}
		   ]},
	case ssam_riakc:put(?BUCKET_MESSAGE_LMS, ref_key(RefId), Doc) of
		ok ->
			state_post_lms(write_log, {RefId, MsgId, ToList, Request});
		{?error, Reason} when is_atom(Reason) ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Reason, Request)};
		{?error, _} ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, write_ref, Request)}
	end;
state_post_lms(write_log, {RefId, MsgId, ToList, Request}) ->
	#request{id = RequestId, account_sid = Sid, body = Body} = Request,
	Subject = proplists:get_value(<<"subject">>, Body, <<>>),
	Body1 = ssam_objects:list_deleted([<<"id">>, <<"to">>, <<"subject">>], Body),
	From = proplists:get_value(<<"from">>, Body1),
	Msg = proplists:get_value(<<"message">>, Body1),
	Doc = {[
			{<<"_ref_id">>, RefId},
			{<<"_request_id">>, RequestId},
			{<<"_type">>, <<"lms">>},
			{<<"_created">>, ssam_util:now_sec()},
			{<<"id">>, MsgId},
			{<<"subject">>, Subject},
			{<<"to">>, [{[{To, <<>>} || To <- ToList]}]} |
			Body1
		]},
	case ssam_riakc:put(?BUCKET_MESSAGE_LMS, log_key(Sid, MsgId), Doc) of
		ok ->
			state_post_lms(dispatch, {RefId, Subject, Msg, From, ToList, Doc, Request});
		{?error, Reason} when is_atom(Reason) ->
			{?error, ssam_rest:response(?STATUS_CONFLICT, Reason, Request)};
		{?error, _} ->
			{?error, ssam_rest:response(
					   ?STATUS_INTERNAL_SERVER_ERROR, write_log, Request)}
	end;
state_post_lms(dispatch, {RefId, Subject, Msg, From, ToList, Doc, Request}) ->
	case ssam_config:value(?SERVICE, [providers, default, handler]) of
		?undefined ->
			{?error, ssam_rest:response(
					   ?STATUS_SERVICE_UNAVAILABLE, ?provider_handler_not_found, Request)};
		Handler when is_atom(Handler) ->
			case Handler:send_lms(RefId, Subject, Msg, From, ToList) of
				ok ->
					{ok, ssam_rest:response(?STATUS_CREATED, Doc, Request)};
				{?error, Reason} when is_atom(Reason) ->
					{?error, ssam_rest:response(
							   ?STATUS_INTERNAL_SERVER_ERROR, Reason, Request)};
				{?error, _} ->
					{?error, ssam_rest:response(
							   ?STATUS_INTERNAL_SERVER_ERROR, dispatch, Request)}
			end
	end.

ref_key(RefId) ->
	<<"ref", ?DELIMITER, RefId/binary>>.

log_key(Sid, MsgId) ->
	<<Sid/binary, ?DELIMITER, MsgId/binary>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

