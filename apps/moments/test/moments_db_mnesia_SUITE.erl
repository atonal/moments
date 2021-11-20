-module(moments_db_mnesia_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([insert_user/1]).

all() -> [insert_user].

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
    {atomic, ok} = moments_db_mnesia:insert_user("uid2", "Name 2"),
    {atomic, {error, user_exists}} = moments_db_mnesia:insert_user("uid1", "Name 3").
