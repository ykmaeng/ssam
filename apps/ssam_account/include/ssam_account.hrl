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

-define(SERVICE, <<"ssam_account">>).

-define(AUTH_KEY_MAX_AGE, 2592000).

-define(invalid_status, invalid_status).
-define(invalid_information, invalid_information).
-define(page_not_found, page_not_found).
-define(user_already_exists, user_already_exists).


-record(account_acl, {
		ip :: binary(),
		filters = [] :: list(binary()),
		family = inet :: inet | inet6
	}).

-record(account_authority, {
		token :: binary(),
		acls = [] :: list(#account_acl{}),
		use = no :: yes | no,
		created :: ssam:timestamp()
	}).

-record(account_user, {
		name :: binary(),
		id :: binary(),
		pw :: binary(),
		sid :: binary(),
		authorities = [] :: [] | list(#account_authority{}),
		props = [] :: [] | ssam:props()
	}).


