-module(moments_db_mnesia_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([insert_user/1,
         insert_moment/1,
         follow/1,
         unfollow/1,
         remove_nonexistent_user/1,
         remove_user/1,
         remove_user_that_follows/1,
         remove_user_remove_admin_moment/1,
         remove_user_set_new_admin_moment/1]).

all() -> [insert_user,
          insert_moment,
          follow,
          unfollow,
          remove_nonexistent_user,
          remove_user,
          remove_user_that_follows,
          remove_user_remove_admin_moment,
          remove_user_set_new_admin_moment].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = test_utils:clear_all_tables(),
    ok.

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    ok = application:start(mnesia),
    ok = application:start(moments),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

% Tests
insert_user(_Config) ->
    {atomic, ok} = moments_db_mnesia:insert_user("uid1", "Name 1"),
    test_utils:verify_user("uid1", "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid2", "Name 2"),
    test_utils:verify_user("uid2", "Name 2"),
    {atomic, {error, user_exists}} = moments_db_mnesia:insert_user("uid1", "Name 3").

insert_moment(_Config) ->
    {atomic, {error, user_doesnt_exists}} = moments_db_mnesia:insert_moment("moment1", "moment name 1", "uid1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid1", "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", "uid1"),
    test_utils:verify_moment("moment1", "moment name 1"),
    test_utils:verify_admin_of("uid1", "moment1"),
    {atomic, {error, moment_exists}} = moments_db_mnesia:insert_moment("moment1", "moment name 2", "uid2").

follow(_Config) ->
    {atomic, {error, moment_doesnt_exists}} = moments_db_mnesia:follow("uid1", "moment1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid1", "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", "uid1"),
    {atomic, {error, user_doesnt_exists}} = moments_db_mnesia:follow("uid2", "moment1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid2", "Name 2"),
    {atomic, ok} = moments_db_mnesia:follow("uid2", "moment1"),
    test_utils:verify_follow("uid2", "moment1").

unfollow(_Config) ->
    {atomic, ok} = moments_db_mnesia:unfollow("uid1", "moment1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid1", "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", "uid1"),
    {atomic, ok} = moments_db_mnesia:insert_user("uid2", "Name 2"),
    {atomic, ok} = moments_db_mnesia:follow("uid2", "moment1"),
    test_utils:verify_follow("uid2", "moment1"),
    {atomic, ok} = moments_db_mnesia:unfollow("uid2", "moment1"),
    test_utils:verify_follow_empty("uid2").

remove_nonexistent_user(_Config) ->
    {atomic, {error, user_doesnt_exists}} = moments_db_mnesia:remove_user("uid1").

remove_user(_Config) ->
    Uid = "uid1",
    {atomic, ok} = moments_db_mnesia:insert_user(Uid, "Name 1"),
    test_utils:verify_user(Uid, "Name 1"),
    {atomic, ok} = moments_db_mnesia:remove_user(Uid),
    test_utils:verify_user_empty(Uid).

remove_user_that_follows(_Config) ->
    Uid1 = "uid1",
    Uid2 = "uid2",
    {atomic, ok} = moments_db_mnesia:insert_user(Uid1, "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", Uid1),
    {atomic, ok} = moments_db_mnesia:insert_user(Uid2, "Name 2"),
    {atomic, ok} = moments_db_mnesia:follow(Uid2, "moment1"),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_follow(Uid2, "moment1"),
    {atomic, ok} = moments_db_mnesia:remove_user(Uid2),
    test_utils:verify_user_empty(Uid2),
    test_utils:verify_follow_empty(Uid2).

remove_user_remove_admin_moment(_Config) ->
    Uid1 = "uid1",
    {atomic, ok} = moments_db_mnesia:insert_user(Uid1, "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", Uid1),
    test_utils:verify_user(Uid1, "Name 1"),
    test_utils:verify_admin_of(Uid1, "moment1"),
    test_utils:verify_moment("moment1", "moment name 1"),
    {atomic, ok} = moments_db_mnesia:remove_user(Uid1),
    test_utils:verify_user_empty(Uid1),
    test_utils:verify_admin_of_empty(Uid1),
    test_utils:verify_moment_empty("moment1").

remove_user_set_new_admin_moment(_Config) ->
    Uid1 = "uid1",
    Uid2 = "uid2",
    {atomic, ok} = moments_db_mnesia:insert_user(Uid1, "Name 1"),
    {atomic, ok} = moments_db_mnesia:insert_moment("moment1", "moment name 1", Uid1),
    {atomic, ok} = moments_db_mnesia:insert_user(Uid2, "Name 2"),
    {atomic, ok} = moments_db_mnesia:follow(Uid2, "moment1"),
    test_utils:verify_user(Uid1, "Name 1"),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid1, "moment1"),
    test_utils:verify_moment("moment1", "moment name 1"),
    {atomic, ok} = moments_db_mnesia:remove_user(Uid1),
    test_utils:verify_user_empty(Uid1),
    test_utils:verify_user(Uid2, "Name 2"),
    test_utils:verify_admin_of(Uid2, "moment1"),
    test_utils:verify_moment("moment1", "moment name 1").
