-module(moments_db_mnesia).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([insert_user/2, remove_user/1]).
-export([dump_table/1, dump_all_tables/0]).

insert_user(Uid, Name) ->
    F = fun() ->
                  case mnesia:read({user, Uid}) =:= [] of
                      true ->
                          User = #user{user_id=Uid, name=Name},
                          mnesia:write(User);
                      false ->
                          ?LOG_ERROR("User ~ts already exists", [Uid]),
                          {error, user_exists}
                  end
          end,
    mnesia:transaction(F).

remove_user(Uid) ->
    F = fun() ->
                  case mnesia:read({user, Uid}) =/= [] of
                      true ->
                          mnesia:delete({user, Uid});
                      false ->
                          ?LOG_ERROR("No user ~ts to delete", [Uid]),
                          {error, user_doesnt_exists}
                  end
        end,
    mnesia:transaction(F).

%% Debug functions

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
