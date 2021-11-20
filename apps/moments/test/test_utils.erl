-module(test_utils).
-export([clear_all_tables/0]).

clear_all_tables()->
    clear_all_tables(mnesia:system_info(tables)).
clear_all_tables([Table|Tail]) when Table =/= schema ->
    {atomic, ok} = mnesia:clear_table(Table),
    clear_all_tables(Tail);
clear_all_tables([_|Tail]) ->
    clear_all_tables(Tail);
clear_all_tables([]) ->
    ok.
