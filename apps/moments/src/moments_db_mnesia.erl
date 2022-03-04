-module(moments_db_mnesia).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/1,
         insert_user/1,
         remove_user/1,
         insert_moment/2,
         insert_moment/7,
         remove_moment/1,
         consume_moment/2,
         get_moment/1,
         get_user/1,
         set_new_admin/2,
         follow/2,
         unfollow/2,
         get_moments/0,
         get_moment_with_links/1,
         get_moments_with_links/0,
         get_moments_with_links/1,
         get_users_with_links/0,
         get_users_with_links/1,
         get_users/0,
         get_user_with_links/1]).

init(Nodes) ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {aborted, {already_exists, schema, Node, disc_copies}} ->
            ?LOG_NOTICE("exists ~p", [Node]);
        {atomic, ok} ->
            ?LOG_NOTICE("schema copy type changed OK"),
            create_tables(Nodes)
    end,
    mnesia:wait_for_tables([moment, user, device, follows, admin_of, owns, table_id], 5000),
    init_table_ids([moment, user, device]).

init_table_ids(TableNames) when is_list(TableNames)->
    lists:foreach(fun init_table_id/1, TableNames).

init_table_id(TableName) ->
    Fun = fun() ->
                  case mnesia:read(table_id, TableName, write) of
                      [] ->
                          mnesia:write(table_id,
                                       #table_id{table_name=TableName, last_id=0},
                                       write);
                      [_] ->
                          ?LOG_DEBUG("table_id ~p already exists", [TableName]),
                          % TODO: verify that the index is correct if it exists
                          ok
                  end
          end,
    {atomic, ok} = mnesia:transaction(Fun).

create_tables(Nodes) ->
    {atomic, ok} = mnesia:create_table(moment, [{attributes, record_info(fields, moment)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)},
                                 {disc_copies, Nodes},
                                 {index, [#user.name]}]),
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
                                 {type, bag}]),
    {atomic, ok} = mnesia:create_table(table_id, [{attributes, record_info(fields, table_id)},
                                 {disc_copies, Nodes}]).


-type db_ret() :: ok | {error, any()}.
-type db_id_ret() :: unique_id() | {error, any()}.

-spec insert_user(user_name()|user()) -> db_id_ret().
insert_user(Name) when is_bitstring(Name) ->
    insert_user(#user{user_id = unknown,
                     name = Name});
insert_user(User = #user{name = Name}) ->
    ?LOG_INFO("Insert user:~p", [User]),
    F = fun() ->
                UserPat = #user{name = Name, _ = '_'},
                Users = mnesia:match_object(UserPat),
                if Users =:= [] ->
                       Uid = generate_unique_id(user),
                       ?LOG_INFO("User ID: ~p", [Uid]),
                       case mnesia:read({user, Uid}) =:= [] of
                           true ->
                               NewUser = User#user{user_id=Uid},
                               mnesia:write(NewUser),
                               Uid;
                           false ->
                               % This shouldn't happen, but hey...
                               ?LOG_ERROR("User with ID ~p already exists", [Uid]),
                               {error, user_exists}
                       end;
                   Users =/= [] ->
                       ?LOG_ERROR("User with name ~p already exists", [Name]),
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


% TODO: do we need this version?
-spec insert_moment(moment_name(), next_moment(), interval(), excl_days(), excl_time(), private(), user_id()) -> db_id_ret().
insert_moment(Name, Next, Interval, ExclDays, ExclTime, Private, Uid) when is_bitstring(Name) ->
    insert_moment(#moment{moment_id=unknown,
                    name=Name,
                    next_moment=Next,
                    interval=Interval,
                    excl_days=ExclDays,
                    excl_time=ExclTime,
                    private=Private},
                  Uid).

-spec insert_moment(moment()|moment_name(), user_id()) -> db_id_ret().
% This is mostly for debug/testing
insert_moment(Name, Uid) when is_bitstring(Name) ->
    ?LOG_INFO("Insert moment name:~p admin:~p", [Name, Uid]),
    insert_moment(Name, erlang:system_time(second)+2, daily, [], [], false, Uid);
insert_moment(Moment, Uid) when is_record(Moment, moment) ->
    ?LOG_INFO("Insert moment:~p admin:~p", [Moment, Uid]),
    F = fun() ->
                case mnesia:read({user, Uid}) =/= [] of
                    true ->
                        Mid = generate_unique_id(moment),
                        ?LOG_INFO("Moment ID: ~p", [Mid]),
                        case mnesia:read({moment, Mid}) =:= [] of
                            true ->
                                NewMoment = Moment#moment{moment_id = Mid},
                                ?LOG_INFO("Adding Moment: ~p", [NewMoment]),
                                mnesia:write(NewMoment),
                                AdminOf = #admin_of{user=Uid, moment=Mid},
                                mnesia:write(AdminOf),
                                Mid;
                            false ->
                                % This shouldn't happen, but hey...
                                ?LOG_ERROR("Moment ~p already exists", [Mid]),
                                {error, moment_exists}
                        end;
                    false ->
                        ?LOG_ERROR("User ~p doesn't exist", [Uid]),
                        {error, user_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res;
insert_moment(_, _) ->
    ?LOG_INFO("Insert moment: no match"),
    {error, invalid_moment}.

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

-spec consume_moment(moment_id(), integer()) -> db_ret().
consume_moment(Mid, DispatchTime) ->
    ?LOG_INFO("consume moment id:~p", [Mid]),
    F = fun() ->
                case mnesia:read(moment, Mid, write) of
                    [M] ->
                        case data_utils:is_passed(M, DispatchTime) of
                                true ->
                                    NewNextMoment = data_utils:get_next_moment(M, DispatchTime),
                                    NewMoment = M#moment{next_moment=NewNextMoment},
                                    mnesia:write(NewMoment);
                                false ->
                                    ?LOG_ERROR("Trying to consume moment before due time! M.next_moment: ~p, dispatch time: ~p, moment: ~p",
                                               [M#moment.next_moment, DispatchTime, Mid]),
                                    {error, dispatch_time_in_future}
                        end;
                    [] ->
                        ?LOG_ERROR("No moment ~p to consume", [Mid]),
                        {error, moment_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-type db_val_ret() :: {ok, any()} | {error, any()}.
-spec get_moment(moment_id()) -> db_val_ret().
get_moment(Mid) ->
    ?LOG_INFO("get moment id:~p", [Mid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) of
                    [M] ->
                        {ok, M};
                    [] ->
                        ?LOG_ERROR("No moment ~p to get", [Mid]),
                        {error, moment_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec get_moment_with_links(moment_id()) -> moment_with_links().
get_moment_with_links(Mid) ->
    ?LOG_INFO("get moment with links, id:~p", [Mid]),
    F = fun() ->
                case mnesia:read({moment, Mid}) of
                    [Moment] ->
                        ?LOG_INFO("found moment:~p", [Moment]),
                        % Produces [follows()]
                        MomentFollows = qlc:q([Follows ||
                                               Follows <- mnesia:table(follows),
                                               Follows#follows.moment =:= Moment#moment.moment_id]),
                        % Produces [admin_of()]
                        MomentAdmins = qlc:q([Admin ||
                                              Admin <- mnesia:table(admin_of),
                                              Admin#admin_of.moment =:= Moment#moment.moment_id]),
                        % Combine as #{moment() => [follows()|admin_of()]}
                        #{Moment => qlc:eval(qlc:append(MomentFollows, MomentAdmins))};
                    [] ->
                        #{}
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

-spec get_moments_with_links() -> moment_with_links().
get_moments_with_links() ->
    get_moments_with_links(fun(_) -> true end).

-spec get_moments_with_links(fun((moment()) -> boolean())) -> moment_with_links().
get_moments_with_links(Pred) ->
    ?LOG_INFO("get moments with links, predicate:~p", [Pred]),
    % Produces [{moment(), []|[follows()]}]
    MomentFollowsPairs = qlc:q([{Moment, if Follows#follows.moment =:= Moment#moment.moment_id ->
                                                [Follows];
                                            true ->
                                                []
                                         end}
                                || Moment <- mnesia:table(moment), Pred(Moment),
                                   Follows <- mnesia:table(follows)],
                               [unique]),
    % Produces [{moment(), []|[admin_of()]}]
    MomentAdminsAdded = qlc:q([{Moment, if Admin#admin_of.moment =:= Moment#moment.moment_id ->
                                                [Admin];
                                            true ->
                                                []
                                         end}
                                || Moment <- mnesia:table(moment), Pred(Moment),
                                   Admin <- mnesia:table(admin_of)],
                               [unique]),
    % folds the above list to #{moment() => [follows()|admin_of()]}
    F = fun() -> G = qlc:fold(
                   fun({Moment, Follows}, Acc) ->
                           maps:update_with(Moment,
                                            fun(FollowList) ->
                                                    FollowList ++ Follows
                                            end,
                                            Follows,
                                            Acc)
                   end,
                   #{},
                   MomentFollowsPairs),
                 A = qlc:fold(
                   fun({Moment, Admin}, Acc) ->
                           maps:update_with(Moment,
                                            fun(FollowList) ->
                                                    FollowList ++ Admin
                                            end,
                                            Admin,
                                            Acc)
                   end,
                   #{},
                   MomentAdminsAdded),
                 {G, A}
        end,
    {atomic, {G, A}} = mnesia:transaction(F),
    maps:merge_with(fun(_K, V1, V2) -> V1 ++ V2 end, G, A).

-spec get_user(user_id()) -> db_val_ret().
get_user(Uid) ->
    ?LOG_INFO("get user id:~p", [Uid]),
    F = fun() ->
                case mnesia:read({user, Uid}) of
                    [M] ->
                        {ok, M};
                    [] ->
                        ?LOG_ERROR("No user ~p to get", [Uid]),
                        {error, user_doesnt_exists}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec get_user_with_links(user_id()) -> user_with_links().
get_user_with_links(Uid) ->
    ?LOG_INFO("get user with links, id:~p", [Uid]),
    F = fun() ->
                case mnesia:read({user, Uid}) of
                    [User] ->
                        ?LOG_INFO("found user:~p", [User]),
                        % Produces [follows()]
                        MomentsFollowed = qlc:q([Follows ||
                                               Follows <- mnesia:table(follows),
                                               Follows#follows.user =:= User#user.user_id]),
                        % Produces [admin_of()]
                        MomentsAdministered = qlc:q([Admin ||
                                              Admin <- mnesia:table(admin_of),
                                              Admin#admin_of.user =:= User#user.user_id]),
                        % Combine as #{user() => [follows()|admin_of()]}
                        #{User => qlc:eval(qlc:append(MomentsFollowed, MomentsAdministered))};
                    [] ->
                        #{}
                end
        end,
    {atomic, Res} = mnesia:transaction(F),
     Res.

-spec get_users() -> [user()].
get_users() ->
    F = fun() ->
                Pat = #user{_ = '_'},
                mnesia:match_object(Pat)
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

-spec get_users_with_links() -> user_with_links().
get_users_with_links() ->
    get_users_with_links(fun(_) -> true end).

-spec get_users_with_links(fun((user()) -> boolean())) -> user_with_links().
get_users_with_links(Pred) ->
    ?LOG_INFO("get users with links"),
    % Produces [{user(), []|[follows()]}]
    UserFollowsPairs = qlc:q([{User, if Follows#follows.user =:= User#user.user_id ->
                                                [Follows];
                                            true ->
                                                []
                                         end}
                                || User <- mnesia:table(user), Pred(User),
                                   Follows <- mnesia:table(follows)],
                               [unique]),
    % Produces [{user(), []|[admin_of()]}]
    UserAdminOfPairs = qlc:q([{User, if Admin#admin_of.user =:= User#user.user_id ->
                                                [Admin];
                                            true ->
                                                []
                                         end}
                                || User <- mnesia:table(user), Pred(User),
                                   Admin <- mnesia:table(admin_of)],
                               [unique]),
    % folds the above list to #{user() => [follows()|admin_of()]}
    F = fun() -> G = qlc:fold(
                   fun({User, Follows}, Acc) ->
                           maps:update_with(User,
                                            fun(FollowList) ->
                                                    FollowList ++ Follows
                                            end,
                                            Follows,
                                            Acc)
                   end,
                   #{},
                   UserFollowsPairs),
                 A = qlc:fold(
                   fun({User, Admin}, Acc) ->
                           maps:update_with(User,
                                            fun(FollowList) ->
                                                    FollowList ++ Admin
                                            end,
                                            Admin,
                                            Acc)
                   end,
                   #{},
                   UserAdminOfPairs),
                 {G, A}
        end,
    {atomic, {G, A}} = mnesia:transaction(F),
    maps:merge_with(fun(_K, V1, V2) -> V1 ++ V2 end, G, A).

-spec generate_unique_id(id_type()) -> unique_id().
generate_unique_id(Type) ->
    % TODO: check that returned id is not in use
    mnesia:dirty_update_counter(table_id, Type, 1).
