-module(test_utils).
-export([clear_all_tables/0, verify_user/2, verify_moment/3, verify_admin_of/2]).

clear_all_tables()->
    clear_all_tables(mnesia:system_info(tables)).
clear_all_tables([Table|Tail]) when Table =/= schema ->
    {atomic, ok} = mnesia:clear_table(Table),
    clear_all_tables(Tail);
clear_all_tables([_|Tail]) ->
    clear_all_tables(Tail);
clear_all_tables([]) ->
    ok.

verify_user(Uid, Name) ->
    F = fun() -> mnesia:read({user, Uid}) end,
    {atomic, [{user, Uid, Name}]} = mnesia:transaction(F).

verify_moment(Mid, Name, Uid) ->
    F = fun() -> mnesia:read({moment, Mid}) end,
    {atomic, [{moment, Mid, Name, 1, none, none, false}]} = mnesia:transaction(F).

verify_admin_of(Uid, Mid) ->
    F = fun() -> mnesia:read({admin_of, Uid}) end,
    {atomic, [{admin_of, Uid, Mid}]} = mnesia:transaction(F).
