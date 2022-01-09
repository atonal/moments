-module(moments_db_mnesia).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/1,
         insert_user/2,
         remove_user/1,
         insert_moment/3,
         remove_moment/1,
         set_new_admin/2,
         follow/2,
         unfollow/2,
         get_moments/0]).

init(Nodes) ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {aborted, {already_exists, schema, Node, disc_copies}} ->
            ?LOG_NOTICE("exists ~p", [Node]);
        {atomic, ok} ->
            ?LOG_NOTICE("schema copy type changed OK"),
            create_tables(Nodes)
    end,
    mnesia:wait_for_tables([moment, user, device, follows, admin_of, owns], 5000).

create_tables(Nodes) ->
    {atomic, ok} = mnesia:create_table(moment, [{attributes, record_info(fields, moment)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(device, [{attributes, record_info(fields, device)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(follows, [{attributes, record_info(fields, follows)},
                                 {disc_copies, Nodes},
                                 {index, [#follows.moment]},
                                 {type, bag}]),
    {atomic, ok} = mnesia:create_table(admin_of, [{attributes, record_info(fields, admin_of)},
                                 {disc_copies, Nodes},
                                 {index, [#admin_of.moment]},
                                 {type, bag}]),
    {atomic, ok} = mnesia:create_table(owns, [{attributes, record_info(fields, owns)},
                                 {disc_copies, Nodes},
                                 {index, [#owns.device]},
                                 {type, bag}]).

-type db_ret() :: ok | {error, any()}.

-spec insert_user(user_id(), user_name()) -> db_ret().
insert_user(Uid, Name) ->
    ?LOG_INFO("Insert user id:~p name:~p", [Uid, Name]),
    F = fun() ->
                case mnesia:read({user, Uid}) =:= [] of
                    true ->
                        User = #user{user_id=Uid, name=Name},
                        mnesia:write(User);
                    false ->
                        ?LOG_ERROR("User ~p already exists", [Uid]),
                        {error, user_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec follow(user_id(), moment_id()) -> db_ret().
follow(Uid, Mid) ->
    ?LOG_INFO("Follow user id:~p moment:~p", [Uid, Mid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) =/= [] of
                    true ->
                        case mnesia:read({user, Uid}) =/= [] of
                            true ->
                                Follow = #follows{user=Uid, moment=Mid},
                                mnesia:write(Follow);
                            false ->
                                ?LOG_ERROR("User ~p doesn't exist", [Uid]),
                                {error, user_doesnt_exists}
                        end;
                    false ->
                        ?LOG_ERROR("Moment ~p doesn't exist", [Mid]),
                        {error, moment_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec unfollow(user_id(), moment_id()) -> db_ret().
unfollow(Uid, Mid) ->
    % TODO: return notification if unfollowing nonexistent things?
    ?LOG_INFO("Unfollow user id:~p moment:~p", [Uid, Mid]),
    F = fun() ->
                mnesia:delete_object({follows, Uid, Mid})
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec remove_user(user_id()) -> db_ret().
remove_user(Uid) ->
    ?LOG_INFO("Remove user id:~p", [Uid]),
    F = fun() ->
                case mnesia:read({user, Uid}) =/= [] of
                    true ->
                        % remove/modify follows
                        case mnesia:read({follows, Uid}) of
                            [] -> ?LOG_ERROR("No moments to follow");
                            MomentsFollowed ->
                                ?LOG_ERROR("MomentsFollowed: ~p", MomentsFollowed),
                                lists:foreach(fun(#follows{moment=Mid}) ->
                                                      ?LOG_ERROR("Moment: ~p", [Mid]),
                                                      ok = unfollow(Uid, Mid)
                                              end, MomentsFollowed)
                        end,
                        % handle administered moments: if no followers, remove moment, otherwise set new admin
                        case mnesia:read({admin_of, Uid}) of
                            [] -> ?LOG_ERROR("No moments to admin");
                            MomentsAdminOf ->
                                ?LOG_ERROR("MomentsAdminOf: ~p", MomentsAdminOf),
                                lists:foreach(fun(#admin_of{moment=Moment}) ->
                                                      ?LOG_ERROR("Moment: ~p", [Moment]),
                                                      mnesia:delete_object({admin_of, Uid, Moment}),

                                                      Pat = #follows{moment = Moment, _ = '_'},
                                                      Followers = mnesia:match_object(Pat),
                                                      AdminPat = #admin_of{moment = Moment, _ = '_'},
                                                      Admins = mnesia:match_object(AdminPat),
                                                      if Admins =:= [] ->
                                                             case Followers of
                                                                 [] ->
                                                                     ok = remove_moment(Moment);
                                                                 [#follows{user=NewAdmin}|_] ->
                                                                     ok = set_new_admin(Moment, NewAdmin)
                                                             end;
                                                         Admins =/= [] ->
                                                             ok
                                                      end
                                              end, MomentsAdminOf)
                        end,
                        mnesia:delete({user, Uid});
                    false ->
                        ?LOG_ERROR("No user ~p to delete", [Uid]),
                        {error, user_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec set_new_admin(moment_id(), user_id()) -> db_ret().
set_new_admin(Mid, NewAdmin) ->
    ?LOG_INFO("Set new admin moment id:~p user id:~p", [Mid, NewAdmin]),
    F = fun() ->
                AdminOf = #admin_of{user=NewAdmin, moment=Mid},
                mnesia:write(AdminOf)
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec insert_moment(moment_id(), moment_name(), user_id()) -> db_ret().
insert_moment(Mid, Name, Uid) ->
    ?LOG_INFO("Insert moment moment id:~p name:~p admin:~p", [Mid, Name, Uid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) =:= [] of
                    true ->
                        case mnesia:read({user, Uid}) =/= [] of
                            true ->
                                Moment = #moment{moment_id=Mid,
                                                 name=Name,
                                                 next_moment=1,
                                                 interval=1,
                                                 excl_days=[],
                                                 excl_time=[],
                                                 private=false},
                                mnesia:write(Moment),
                                AdminOf = #admin_of{user=Uid, moment=Mid},
                                mnesia:write(AdminOf);
                            false ->
                                ?LOG_ERROR("User ~p doesn't exist", [Uid]),
                                {error, user_doesnt_exists}
                        end;
                    false ->
                        ?LOG_ERROR("Moment ~p already exists", [Mid]),
                        {error, moment_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec remove_moment(moment_id()) -> db_ret().
remove_moment(Mid) ->
    ?LOG_INFO("Remove moment id:~p", [Mid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) =/= [] of
                    true ->
                        mnesia:delete({moment, Mid}),
                        % remove admin_of link
                        AdminPat = #admin_of{moment = Mid, _ = '_'},
                        Delete = mnesia:match_object(AdminPat),
                        lists:foreach(fun(X) ->
                                              mnesia:delete_object(X)
                                      end, Delete),
                        % remove follows links
                        FollowPat = #follows{moment = Mid, _ = '_'},
                        Followers = mnesia:match_object(FollowPat),
                        lists:foreach(fun(X) ->
                                              mnesia:delete_object(X)
                                      end, Followers);
                    false ->
                        ?LOG_ERROR("No moment ~p to delete", [Mid]),
                        {error, moment_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec get_moments() -> [moment()].
get_moments() ->
    F = fun() ->
                Pat = #moment{_ = '_'},
                mnesia:match_object(Pat)
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.
