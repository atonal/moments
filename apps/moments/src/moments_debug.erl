-module(moments_debug).

-export([dump_table/1, dump_all_tables/0]).

dump_table(Tab) ->
    Wildpattern = mnesia:table_info(Tab, wild_pattern),
    F = fun() -> mnesia:match_object(Wildpattern) end,
    {atomic, Table} = mnesia:transaction(F),
    Table.

dump_all_tables()->
    dump_all_tables(mnesia:system_info(tables), []).

dump_all_tables([Table|Tail], Tables) when Table =/= schema ->
    dump_all_tables(Tail, Tables ++ dump_table(Table));
dump_all_tables([_|Tail], Tables) ->
    dump_all_tables(Tail, Tables);
dump_all_tables([], Tables) ->
    Tables.