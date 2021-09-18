-module(moments_db_mnesia).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([insert_user/2, remove_user/1, insert_moment/3, remove_moment/1, set_new_admin/2]).
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
                        % TODO: remove/modify follows
                        mnesia:delete({user, Uid}),
                        case mnesia:read({admin_of, Uid}) of
                            [] -> ?LOG_ERROR("No moments to admin");
                            MomentsAdminOf ->
                                ?LOG_ERROR("MomentsAdminOf: ~ts", MomentsAdminOf),
                                lists:foreach(fun(#admin_of{moment=Moment}) ->
                                                      ?LOG_ERROR("Moment: ~ts", [Moment]),
                                                      % Moment = #admin_of{moment = Moment, _ = '_'},
                                                      Pat = #follows{moment = Moment, _ = '_'},
                                                      Followers = mnesia:match_object(Pat),
                                                      case Followers of
                                                          [] -> remove_moment(Moment);
                                                          [#follows{user=NewAdmin}|_] ->
                                                              mnesia:delete_object({admin_of, Uid, Moment}),
                                                              set_new_admin(Moment, NewAdmin)
                                                      end
                                              end, MomentsAdminOf)
                        end;
                    false ->
                        ?LOG_ERROR("No user ~ts to delete", [Uid]),
                        {error, user_doesnt_exists}
                end
        end,
    mnesia:transaction(F).

set_new_admin(Mid, NewAdmin) ->
    F = fun() ->
                AdminOf = #admin_of{user=NewAdmin, moment=Mid},
                mnesia:write(AdminOf)
        end,
    mnesia:transaction(F).

insert_moment(Mid, Name, Uid) ->
    F = fun() ->
                case mnesia:read({moment, Mid}) =:= [] of
                    true ->
                        case mnesia:read({user, Uid}) =/= [] of
                            true ->
                                Moment = #moment{moment_id=Mid,
                                                 name=Name,
                                                 interval=1,
                                                 excl_days=none,
                                                 excl_time=none,
                                                 private=false},
                                mnesia:write(Moment),
                                AdminOf = #admin_of{user=Uid, moment=Mid},
                                mnesia:write(AdminOf);
                            false ->
                                ?LOG_ERROR("User ~ts doesn't exist", [Uid]),
                                {error, user_doesnt_exists}
                        end;
                    false ->
                        ?LOG_ERROR("Moment ~ts already exists", [Mid]),
                        {error, user_exists}
                end
        end,
    mnesia:transaction(F).

remove_moment(Mid) ->
    ?LOG_ERROR("Remove moment ~ts", [Mid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) =/= [] of
                    true ->
                        mnesia:delete({moment, Mid}),
                        Pat = #admin_of{moment = Mid, _ = '_'},
                        Delete = mnesia:match_object(Pat),
                        lists:foreach(fun(X) ->
                                              mnesia:delete_object(X)
                                      end, Delete);
                    false ->
                        ?LOG_ERROR("No moment ~ts to delete", [Mid]),
                        {error, moment_doesnt_exists}
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
