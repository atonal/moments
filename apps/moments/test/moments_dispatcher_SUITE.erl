-module(moments_dispatcher_SUITE).
-include("../src/data_records.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [init_with_one_moment].

init_per_testcase(_, Config) ->
    meck:new(moments_db_mnesia),
    meck:new(moment_dispatch),
    Config.

end_per_testcase(_, _Config) ->
    true = meck:validate(moment_dispatch),
    true = meck:validate(moments_db_mnesia),
    meck:unload(moment_dispatch),
    meck:unload(moments_db_mnesia),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

% Tests
init_with_one_moment(_Config) ->
    meck:expect(moments_db_mnesia, get_moments, 0,
                meck:seq([[#moment{ next_moment=erlang:system_time(second)+1 }], []])),
    meck:expect(moment_dispatch, dispatch, 1, ok),
    _Pid = moment_dispatcher:start_link(),
    true = meck:called(moments_db_mnesia, get_moments, []),
    ok = meck:wait(moment_dispatch, dispatch, 1, 5000),
    ok = moment_dispatcher:stop().
