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

-module(ssam_resource).

-export([
		 get_tree/3,
		 get/3,
		 put/4,
		 post/4,
		 delete/3,
		 bucket/2
		]).

-include_lib("ssam/include/ssam.hrl").

%% Public functions

get_tree(_, _, []) ->
	{?error, ?invalid_collection_name};
get_tree(_, _, [{[], _} | _]) ->
	{?error, ?invalid_collection_name};
get_tree(Key, Service, [{Collection, []} | _]) ->
	case get_collection(Key, Service, Collection) of
		{?error, Reason} ->
			{?error, {Reason, Collection, []}};
		{ok, Items} when is_list(Items) ->
			{ok, [{Items, []}]};
		_ ->
			{?error, {?invalid_data, Collection, []}}
	end;
get_tree(Key, Service, [{Collection, Id} | Rest]) ->
	case get_collection_item(Key, Service, Collection, Id) of
		{?error, Reason} ->
			{?error, {Reason, Collection, Id}};
		{ok, {Items, Item}} ->
			case get_tree(Rest, [{Items, Item}]) of
				{ok, Output} ->
					{ok, Output};
				{?error, {Reason, ErrColl, ErrId}} ->
					{?error, {Reason, ErrColl, ErrId}}
			end
	end;
get_tree(Key, Service, Uri) when is_list(Uri) ->
	get_tree(Key, Service, path(Uri)).

get_tree([], Output) -> {ok, Output};
get_tree([{Collection, Id} | Rest], Output) ->
	[{_, Item} | _] = Output,
	case ssam_objects:value(Collection, Item) of
		?undefined ->
			{?error, {?invalid_collection_name, Collection, Id}};
		SubItems when Id =:= [] ->
			Output1 = [{SubItems, []} | Output],
			{ok, Output1};
		SubItems ->
			case ssam_objects:object(<<"id">>, Id, SubItems) of
				[] ->
					{?error, {?invalid_resource_id, Collection, Id}};
				SubItem ->
					Output1 = [{SubItems, SubItem} | Output],
					get_tree(Rest, Output1)
			end
	end.




get(_, _, []) ->
	{?error, {?invalid_collection_name, [], []}};
get(_, _, [{[], _} | _]) ->
	{?error, {?invalid_collection_name, [], []}};
get(Key, Service, [{Collection, []} | _]) ->
	case get_collection(Key, Service, Collection) of
		{?error, Reason} ->
			{?error, {Reason, Collection, []}};
		{ok, Items} when is_list(Items) ->
			{ok, Items};
		_ ->
			{?error, {?invalid_data, Collection, []}}
	end;
get(Key, Service, [{Collection, Id} | Rest]) ->
	case get_item(Key, Service, Collection, Id) of
		{?error, Reason} ->
			{?error, {Reason, Collection, Id}};
		{ok, Item} ->
			case get(Rest, Item) of
				{ok, Output} ->
					{ok, Output};
				{?error, {Reason, ErrColl, ErrId}} ->
					{?error, {Reason, ErrColl, ErrId}}
			end
	end;
get(Key, Service, Uri) when is_list(Uri) ->
	get(Key, Service, path(Uri)).

get([], Output) -> {ok, Output};
get([{Collection, Id} | Rest], Item) ->
	case ssam_objects:value(Collection, Item) of
		?undefined ->	
			{?error, {?invalid_collection_name, Collection, Id}};
		SubItems when Id =:= [] ->
			{ok, SubItems};
		SubItems ->
			case ssam_objects:object(<<"id">>, Id, SubItems) of
				[] ->
					{?error, {?invalid_resource_id, Collection, Id}};
				SubItem ->
					get(Rest, SubItem)
			end
	end.


post(Key, Service, [{Collection, Id} | _] = Path, NewItem) ->
	Bucket = bucket(Service, Collection),
	case get_tree(Key, Service, Path) of
		{?error, {?invalid_collection_name, _, _}} when Id =:= [] ->
			NewItem1 = case NewItem of [] -> []; _ -> [NewItem] end,
			case riak_put(Bucket, Key, NewItem1) of
				ok ->
					{ok, [NewItem]};
				{error, Reason} ->
					{?error, {Reason, Collection, Id}}
			end;
		{?error, {Reason, ErrColl, ErrId}} ->
			{?error, {Reason, ErrColl, ErrId}};
		{ok, GotItems} ->
			[{Coll, []} | PathTail] = lists:reverse(Path),
			[{Items, []} | ItemTail] = GotItems,
			case ssam_objects:value(<<"id">>, NewItem) of
				?undefined ->
					{?error, {?invalid_resource_id, Coll, []}};
				NewId ->
					case ssam_objects:object(<<"id">>, NewId, Items) of
						[] ->
							NewItems = ssam_objects:appended(NewItem, Items),
							case apply_changes(PathTail, ItemTail, {Coll, NewItems}) of
								{?error, {Reason, ErrColl, ErrId}} ->
									{?error, {Reason, ErrColl, ErrId}};
								{ok, Output} ->
									update_db(Bucket, Key, Output)
							end;
						_ ->
							{?error, {?duplicate_resource_id, Coll, NewId}}
					end
			end
	end;
post(Key, Service, Uri, NewItem) when is_list(Uri) ->
	post(Key, Service, path(Uri), NewItem).


put(Key, Service, [{Collection, _Id} | _] = Path, NewItem) ->
	Bucket = bucket(Service, Collection),
	case get_tree(Key, Service, Path) of
		{?error, {Reason, ErrColl, ErrId}} ->
			{?error, {Reason, ErrColl, ErrId}};
		{ok, GotItems} ->
			[{Coll, Id} | PathTail] = lists:reverse(Path),
			[{Items, Item} | ItemTail] = GotItems,
			NewItem1 = ssam_objects:value_replaced(<<"id">>, Id, NewItem),
			NewItems = ssam_objects:replaced(Item, NewItem1, Items),
			case apply_changes(PathTail, ItemTail, {Coll, NewItems}) of
				{?error, {Reason, ErrColl, ErrId}} ->
					{?error, {Reason, ErrColl, ErrId}};
				{ok, Output} ->
					update_db(Bucket, Key, Output)
			end
	end;
put(Key, Service, Uri, NewItem) when is_list(Uri) ->
	put(Key, Service, path(Uri), NewItem).


delete(Key, Service, [{Collection, _} | _] = Path) ->
	Bucket = bucket(Service, Collection),
	case get_tree(Key, Service, Path) of
		{?error, {Reason, ErrColl, ErrId}} ->
			{?error, {Reason, ErrColl, ErrId}};
		{ok, GotItems} ->
			[{Coll, _} | PathTail] = lists:reverse(Path),
			[{Items, Item} | ItemTail] = GotItems,
			Deleted = case Item =:= [] of
				true -> [];
				false -> ssam_objects:deleted(Item, Items)
			end,
			case apply_changes(PathTail, ItemTail, {Coll, Deleted}) of
				{?error, {Reason, ErrColl, ErrId}} ->
					{?error, {Reason, ErrColl, ErrId}};
				{ok, Output} ->
					update_db(Bucket, Key, Output)
			end
	end;
delete(Key, Service, Uri) when is_list(Uri) ->
	delete(Key, Service, path(Uri)).


%% Private functions

apply_changes([], [], {_, Output}) ->
	{ok, Output};
apply_changes([{Coll, Id} | PathTail], [{OldItems, OldItem} | ItemTail], {Key, NewVal}) ->
	NewItem = ssam_objects:value_replaced(Key, NewVal, OldItem),
	case ssam_objects:replaced(OldItem, NewItem, OldItems) of
		[] ->
			{?error, {?invalid_data, Coll, Id}};
		NewItems ->
			apply_changes(PathTail, ItemTail, {Coll, NewItems})
	end.



update_db(Bucket, Key, Val) ->
	case riak_put(Bucket, Key, Val) of
		ok ->
			{ok, Val};
		{error, Reason} ->
			{?error, {Reason, [], []}}
	end.


%% @doc GET the collection.
get_collection(Key, Service, Collection) ->
	Bucket = bucket(Service, Collection),
	case ssam_riakc:get(Bucket, Key) of
		{?error, ?not_found} ->
			%ssam_monitor:notify(error, ?not_found),
			{?error, ?invalid_collection_name};
		{?error, _Reason} ->
			%% @todo ssam_monitor:notify(error, Reason),
			{?error, ?conflict};
		{ok, Bin} when is_binary(Bin) ->
			{ok, binary_to_term(Bin)};
		{ok, Doc} ->
			{ok, Doc}
	end.

%% @doc GET the item.
get_collection_item(Key, Service, Collection, ItemId) ->
	case get_collection(Key, Service, Collection) of
		{?error, Reason} ->
			{?error, Reason};
		{ok, Items} ->
			case ssam_objects:object(<<"id">>, ItemId, Items) of
				[] ->
					{?error, ?invalid_resource_id};
				Item ->
					{ok, {Items, Item}}
			end
	end.

%% @doc GET the item.
get_item(Key, Service, Collection, ItemId) ->
	case get_collection_item(Key, Service, Collection, ItemId) of
		{?error, Reason} ->
			{?error, Reason};
		{ok, {_, Item}} ->
			{ok, Item}
	end.

bucket(Service, Collection) ->
	<<Service/binary, $_, Collection/binary>>.


riak_put(Bucket, Key, Val) when is_binary(Val) ->
	ssam_riakc:put(Bucket, Key, Val);
riak_put(Bucket, Key, Val) ->
	ssam_riakc:put(Bucket, Key, term_to_binary(Val)).


path([]) -> [];
path(L) when not is_list(L) -> [];
path(L) -> lists:reverse(path(L, [], [])).

path([H | []], [], Pairs) -> [{H, []} | Pairs];
path([H | T], [], Pairs) -> path(T, H, Pairs);
path([H | []], Val, Pairs) -> [{Val, H} | Pairs];
path([H | T], Val, Pairs) -> path(T, [], [{Val, H} | Pairs]).







-ifdef(TEST_).
-include_lib("eunit/include/eunit.hrl").

account_sid() -> <<"test_account_sid">>.
service() -> <<"test_service">>.
collection() -> <<"test_collection">>.

init_data() ->
	Bucket = bucket(service(), collection()),
	ok = ssam_riakc:delete(Bucket, account_sid()).

put_test_data(Data) ->
	Bucket = bucket(service(), collection()),
	ok = riak_put(Bucket, account_sid(), Data).


get_collection_test_() ->
	[
	 {"ok",
	  fun() ->
		  ok = init_data(),
		  CollData = [],
		  ok = put_test_data(CollData),
		  {ok, CollData} = get_collection(account_sid(), service(), collection())
	  end
	 },
	 {"error, invalid_collection_name",
	  fun() ->
		  ok = init_data(),
		  {?error, ?invalid_collection_name}
		  = get_collection(account_sid(), service(), collection())
	  end
	 }
	].

get_collection_item_test_() ->
	[
	 {"ok",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"data">>, <<"2">>}]},
		  CollData = [Data1, Data2],
		  ok = put_test_data(CollData),
		  {ok, {CollData, Data1}} =
			  get_collection_item(account_sid(), service(), collection(), <<"1">>),
		  {ok, {CollData, Data2}} =
			  get_collection_item(account_sid(), service(), collection(), <<"2">>)
	  end
	 },
	 {"error, invalid_collection_name",
	  fun() ->
		  ok = init_data(),
		  Id = <<"1">>,
		  {?error, ?invalid_collection_name} =
			  get_collection_item(account_sid(), service(), collection(), Id)
	  end
	 },
	 {"error, invalid_resource_id when collection empty",
	  fun() ->
		  ok = init_data(),
		  ok = put_test_data([]),
		  {?error, ?invalid_resource_id} =
			  get_collection_item(account_sid(), service(), collection(), <<"1">>)
	  end
	 },
	 {"error, invalid_resource_id when id not matched",
	  fun() ->
		  ok = init_data(),
		  CollData = [{[{<<"id">>, <<"not matched">>}]}], Id = <<"1">>,
		  ok = put_test_data(CollData),
		  {?error, ?invalid_resource_id} =
			  get_collection_item(account_sid(), service(), collection(), Id)
	  end
	 }
	].

get_test_() ->
	[
	 {"ok, only collection",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  CollData = [Data1, {[]}],
		  ok = put_test_data(CollData),
		  {ok, CollData} =
		  get(account_sid(), service(), [{collection(), []}]),
		  {ok, [{CollData, []}]} =
		  get_tree(account_sid(), service(), [{collection(), []}])

	  end
	 },
	 {"ok, both collection and id",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"data">>, <<"2">>}]},
		  CollData = [Data1, Data2],
		  ok = put_test_data(CollData),
		  {ok, Data1} =
		  get(account_sid(), service(), [{collection(), <<"1">>}]),
		  {ok, Data2} =
		  get(account_sid(), service(), [{collection(), <<"2">>}]),

		  {ok, [{CollData, Data1}]} =
		  get_tree(account_sid(), service(), [{collection(), <<"1">>}]),
		  {ok, [{CollData, Data2}]} =
		  get_tree(account_sid(), service(), [{collection(), <<"2">>}])
	  end
	 },
	 {"ok, depth 2 expect id",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"subcoll">>, []}]},
		  Data3 = {[{<<"id">>, <<"3">>}, {<<"subcoll">>, <<"subdata">>}]},
		  CollData = [Data1, Data2, Data3],
		  ok = put_test_data(CollData),
		  {ok, []} = get(account_sid(), service(),
									[{collection(), <<"2">>}, {<<"subcoll">>, []}]),
		  {ok, <<"subdata">>} = get(account_sid(), service(),
								   [{collection(), <<"3">>}, {<<"subcoll">>, []}]),

		  {ok, Result2} = get_tree(account_sid(), service(),
								   [{collection(), <<"2">>}, {<<"subcoll">>, []}]),
		  [{[], []}, {CollData, Data2}] = Result2,
		  {ok, Result3} = get_tree(account_sid(), service(),
								   [{collection(), <<"3">>}, {<<"subcoll">>, []}]),
		  [{<<"subdata">>, []}, {CollData, Data3}] = Result3
	  end
	 },
	 {"error, invalid_collection_name",
	  fun() ->
		  ok = init_data(),
		  Coll = collection(), Id = <<"1">>,
		  {?error, {?invalid_collection_name, Coll, Id}} =
		  get(account_sid(), service(), [{Coll, Id}]),
		  {?error, {?invalid_collection_name, Coll, Id}} =
		  get_tree(account_sid(), service(), [{Coll, Id}])
	  end
	 },
	 {"error, invalid_resource_id when data empty",
	  fun() ->
		  ok = init_data(),
		  ok = put_test_data([]),
		  Coll = collection(), Id = <<"1">>,
		  {?error, {?invalid_resource_id, Coll, Id}} =
		  get(account_sid(), service(), [{Coll, Id}]),
		  {?error, {?invalid_resource_id, Coll, Id}} =
		  get_tree(account_sid(), service(), [{Coll, Id}])
	  end
	 },
	 {"error, invalid_resource_id when id not matched",
	  fun() ->
		  ok = init_data(),
		  Coll = collection(), Id = <<"1">>,
		  CollData = [{[{<<"id">>, <<"not matched">>}]}], 
		  ok = put_test_data(CollData),
		  {?error, {?invalid_resource_id, Coll, Id}} =
		  get(account_sid(), service(), [{Coll, Id}]),
		  {?error, {?invalid_resource_id, Coll, Id}} =
		  get_tree(account_sid(), service(), [{Coll, Id}])
	  end
	 },
	 {"error, invalid_collection_name when collection not matched",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"subcoll">>, [{<<"id">>, <<"sub1">>}]}]},
		  CollData = [Data1, Data2],
		  ok = put_test_data(CollData),
		  {?error, {?invalid_collection_name, <<"wrong">>, []}} =
		  get(account_sid(), service(), [{collection(), <<"2">>}, {<<"wrong">>, []}]),
		  {?error, {?invalid_collection_name, <<"wrong">>, []}} =
		  get_tree(account_sid(), service(), [{collection(), <<"2">>}, {<<"wrong">>, []}])
	  end
	 },
	 {"error, invalid_resource_id when id not matched on multiple data",
	  fun() ->
		  ok = init_data(),
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"subcoll">>, [{[
															{<<"id">>, <<"sub1">>},
															{<<"data">>, <<"1">>}
														   ]}]
										 }]},
		  CollData = [Data1, Data2],
		  ok = put_test_data(CollData),
		  {?error, {?invalid_resource_id, <<"subcoll">>, <<"invalid">>}} =
		  get(account_sid(), service(), [{collection(), <<"2">>},
										 {<<"subcoll">>, <<"invalid">>}])
	  end
	 }
	].


post_test_() ->
	[
	 {"ok, post a new resource on empty collection",
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),
		  {ok, [{[Data], []}]} = get_tree(account_sid(), service(), Path)
	  end
	 },
	 {"ok, post a new resource when collections already exist",
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"subcoll">>, []}]},
		  {ok, [Data1]} = post(account_sid(), service(), Path, Data1),
		  
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"data">>, <<"2">>}]},
		  {ok, [Data2, Data1]} = post(account_sid(), service(), Path, Data2),

		  Data3 = {[{<<"id">>, <<"11">>}, {<<"data">>, <<"11">>}]},
		  Data11 = {[{<<"subcoll">>, [Data3]}, {<<"id">>, <<"1">>}]},
		  {ok, [Data11, Data2]}
			= post(account_sid(), service(),
				   [{collection(), <<"1">>}, {<<"subcoll">>, []}], Data3)
	  end
	 },
	 {"error, duplicate_resource_id",
	  fun() ->
		  ok = init_data(),
		  Path = [{Coll = collection(), []}],
		  Data = {[{<<"id">>, <<"duplicate">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),
		  {?error, {?duplicate_resource_id, Coll, <<"duplicate">>}} =
		  post(account_sid(), service(), Path, Data)
	  end
	 },
	 {"error, invalid_collection_name",
	  fun() ->
		  ok = init_data(),
		  Path = [{Coll = collection(), <<"1">>}, {<<"data">>, []}],
		  {?error, {?invalid_collection_name, Coll, <<"1">>}}
		  = post(account_sid(), service(), Path, []),

		  InputPath = [{collection(), []}],
		  InputData1 = {[{<<"id">>, <<"1">>}, {<<"data">>, []}]},
		  {ok, [InputData1]} = post(account_sid(), service(), InputPath, InputData1),

		  InvalidPath = [{Coll, <<"1">>}, {<<"invalid">>, []}],
		  {?error, {?invalid_collection_name, <<"invalid">>, []}}
		  = post(account_sid(), service(), InvalidPath, <<"some data">>)
	  end
	 },
	 {"error, invalid_resource_id",
	  fun() ->
		  ok = init_data(),
		  Path = [{Coll = collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data1">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),

		  Path1 = [{Coll, <<"invalid">>}, {<<"data2">>, []}],
		  {?error, {?invalid_resource_id, Coll, <<"invalid">>}}
		  = post(account_sid(), service(), Path1, <<"some data">>)

	  end
	 }
	].

put_test_() ->
	[
	 {"ok, post and put", 
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),

		  Path1 = [{collection(), <<"1">>}],
		  Data1 = {[{<<"data">>, <<"changed">>}, {<<"id">>, <<"1">>}]},
		  {ok, [{[{<<"id">>, <<"1">>}, {<<"data">>, <<"changed">>}]}]}
			= put(account_sid(), service(), Path1, Data1)
	  end
	 },
	 {"error, invalid id", 
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),

		  Path1 = [{Coll = collection(), <<"invalid">>}],
		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"changed">>}]},
		  {?error, {?invalid_resource_id, Coll, <<"invalid">>}} =
		  put(account_sid(), service(), Path1, Data1)
	  end
	 }
	].

delete_test_() ->
	[
	 {"ok, post and delete", 
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),

		  Path1 = [{collection(), <<"1">>}],
		  {ok, []} = delete(account_sid(), service(), Path1),

		  Data1 = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  Data2 = {[{<<"id">>, <<"2">>}, {<<"data">>, <<"2">>}]},
		  {ok, [Data1]} = post(account_sid(), service(), Path, Data1),
		  {ok, [Data2, Data1]} = post(account_sid(), service(), Path, Data2),
		  {ok, [Data2]} = delete(account_sid(), service(), Path1)
	  end
	 },
	 {"error, invalid id", 
	  fun() ->
		  ok = init_data(),
		  Path = [{collection(), []}],
		  Data = {[{<<"id">>, <<"1">>}, {<<"data">>, <<"1">>}]},
		  {ok, [Data]} = post(account_sid(), service(), Path, Data),

		  Path1 = [{Coll = collection(), <<"invalid">>}],
		  {?error, {?invalid_resource_id, Coll, <<"invalid">>}} =
		  delete(account_sid(), service(), Path1)
	  end
	 }
	].

path_test_() ->
	[
	 {"test",
	  fun() ->
		  [{<<"a">>, []}] = path([<<"a">>]),
		  [{<<"a">>, <<"1">>}] = path([<<"a">>, <<"1">>]),
		  [{<<"a">>, <<"1">>}, {<<"b">>, []}] = path([<<"a">>, <<"1">>, <<"b">>]),
		  [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}] =
		  path([<<"a">>, <<"1">>, <<"b">>, <<"2">>])
	  end
	 }
	].

-endif.




