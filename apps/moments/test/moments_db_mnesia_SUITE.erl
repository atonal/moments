-module(moments_db_mnesia_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/data_records.hrl").

-compile(export_all).

all() -> [insert_user,
          insert_existing_user,
          insert_moment,
          insert_moment_user_doesnt_exist,
          follow_moment_doesnt_exist,
          follow_user_doesnt_exist,
          follow,
          unfollow_nonexistent,
          unfollow,
          remove_nonexistent_user,
          remove_user,
          remove_user_that_follows,
          remove_user_remove_admin_moment,
          remove_user_set_new_admin_moment,
          remove_user_dont_set_new_admin_moment,
          set_new_admin,
          remove_nonexistent_moment,
          remove_moment,
          remove_moment_remove_follows,
          get_moment,
          consume_moment,
          consume_moment_ahead_of_time
         ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = test_utils:clear_all_tables(),
    ok.

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    ok = application:start(mnesia),
    ok = moments_db_mnesia:init([node()]),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

% Tests
insert_user(_Config) ->
    Uid = moments_db_mnesia:insert_user("Name 1"),
    test_utils:verify_user(Uid, "Name 1").

insert_existing_user(_Config) ->
    Uid = moments_db_mnesia:insert_user("Name 1"),
    test_utils:verify_user(Uid, "Name 1"),
    {error, user_exists} = moments_db_mnesia:insert_user("Name 1").

insert_moment_user_doesnt_exist(_Config) ->
    {error, user_doesnt_exists} = moments_db_mnesia:insert_moment("moment name 1", "uid1").

insert_moment(_Config) ->
    Uid = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid),
    test_utils:verify_moment(Mid, "moment name 1"),
    test_utils:verify_admin_of(Uid, Mid).

follow_moment_doesnt_exist(_Config) ->
    {error, moment_doesnt_exists} = moments_db_mnesia:follow("uid1", "moment1").

follow_user_doesnt_exist(_Config) ->
    Uid = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid),
    {error, user_doesnt_exists} = moments_db_mnesia:follow("uid2", Mid).

follow(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:follow(Uid2, Mid),
    test_utils:verify_follow(Uid2, Mid).

unfollow_nonexistent(_Config) ->
    ok = moments_db_mnesia:unfollow("uid1", "moment1").

unfollow(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:follow(Uid2, Mid),
    test_utils:verify_follow(Uid2, Mid),
    ok = moments_db_mnesia:unfollow(Uid2, Mid),
    test_utils:verify_follow_empty(Uid2).

remove_nonexistent_user(_Config) ->
    {error, user_doesnt_exists} = moments_db_mnesia:remove_user("uid1").

remove_user(_Config) ->
    Uid = moments_db_mnesia:insert_user("Name 1"),
    test_utils:verify_user(Uid, "Name 1"),
    ok = moments_db_mnesia:remove_user(Uid),
    test_utils:verify_user_empty(Uid).

remove_user_that_follows(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:follow(Uid2, Mid),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_follow(Uid2, Mid),
    ok = moments_db_mnesia:remove_user(Uid2),
    test_utils:verify_user_empty(Uid2),
    test_utils:verify_follow_empty(Uid2).

remove_user_remove_admin_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    test_utils:verify_user(Uid1, "Name 1"),
    test_utils:verify_admin_of(Uid1, Mid),
    test_utils:verify_moment(Mid, "moment name 1"),
    ok = moments_db_mnesia:remove_user(Uid1),
    test_utils:verify_user_empty(Uid1),
    test_utils:verify_admin_of_empty(Uid1),
    test_utils:verify_moment_empty(Mid).

% TODO: _which_ user is set as new admin?
remove_user_set_new_admin_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:follow(Uid2, Mid),
    test_utils:verify_user(Uid1, "Name 1"),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid1, Mid),
    test_utils:verify_moment(Mid, "moment name 1"),
    ok = moments_db_mnesia:remove_user(Uid1),
    test_utils:verify_user_empty(Uid1),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid2, Mid),
    test_utils:verify_moment(Mid, "moment name 1").

remove_user_dont_set_new_admin_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:set_new_admin(Mid, Uid2),
    test_utils:verify_user(Uid1, "Name 1"),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid1, Mid),
    test_utils:verify_admin_of(Uid2, Mid),
    test_utils:verify_moment(Mid, "moment name 1"),
    ok = moments_db_mnesia:remove_user(Uid1),
    test_utils:verify_user_empty(Uid1),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid2, Mid),
    test_utils:verify_moment(Mid, "moment name 1").

set_new_admin(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    test_utils:verify_admin_of(Uid1, Mid),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:set_new_admin(Mid, Uid2),
    test_utils:verify_admin_of(Uid2, Mid).

remove_nonexistent_moment(_Config) ->
    {error, moment_doesnt_exists} = moments_db_mnesia:remove_moment("moment1").

remove_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    test_utils:verify_moment(Mid, "moment name 1"),
    test_utils:verify_admin_of(Uid1, Mid),
    ok = moments_db_mnesia:remove_moment(Mid),
    test_utils:verify_moment_empty(Mid),
    test_utils:verify_admin_of_empty(Uid1).

remove_moment_remove_follows(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    Uid2 = moments_db_mnesia:insert_user("Name 2"),
    ok = moments_db_mnesia:follow(Uid2, Mid),
    test_utils:verify_follow(Uid2, Mid),
    ok = moments_db_mnesia:remove_moment(Mid),
    test_utils:verify_follow_empty(Uid2).

get_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    {ok, #moment{moment_id = Mid}} = moments_db_mnesia:get_moment(Mid).

consume_moment(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    {ok, Moment} = moments_db_mnesia:get_moment(Mid),
    NextMoment = Moment#moment.next_moment,
    ok = moments_db_mnesia:consume_moment(Mid, NextMoment + 1),
    {ok, NewMoment} = moments_db_mnesia:get_moment(Mid),
    NewMoment = Moment#moment{next_moment = NextMoment + 86_400}.

consume_moment_ahead_of_time(_Config) ->
    Uid1 = moments_db_mnesia:insert_user("Name 1"),
    Mid = moments_db_mnesia:insert_moment("moment name 1", Uid1),
    {ok, Moment} = moments_db_mnesia:get_moment(Mid),
    NextMoment = Moment#moment.next_moment,
    {error, dispatch_time_in_future} = moments_db_mnesia:consume_moment(Mid, NextMoment - 1).
