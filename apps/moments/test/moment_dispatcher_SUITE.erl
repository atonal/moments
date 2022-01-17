-module(moment_dispatcher_SUITE).
-include("../src/data_records.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [init_with_one_moment,
          add_one_moment_to_empty_queue,
          add_one_moment_in_front_of_existing_moment
         ].

init_per_testcase(_, Config) ->
    meck:new(moment_dispatcher, [passthrough]),
    meck:new(moments_db_mnesia),
    meck:new(moment_dispatch),
    Config.

end_per_testcase(_, _Config) ->
    true = meck:validate(moment_dispatch),
    true = meck:validate(moments_db_mnesia),
    true = meck:validate(moment_dispatcher),
    meck:unload(moment_dispatcher),
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
    meck:expect(moment_dispatch, dispatch, 2, ok),
    _Pid = moment_dispatcher:start_link(),
    1 = length(moment_dispatcher:get_queue()),
    true = meck:called(moments_db_mnesia, get_moments, []),
    ok = meck:wait(1, moment_dispatch, dispatch, 2, 5000),
    0 = length(moment_dispatcher:get_queue()),
    ok = moment_dispatcher:stop().

add_one_moment_to_empty_queue(_Config) ->
    meck:expect(moments_db_mnesia, get_moments, 0, []),
    meck:expect(moment_dispatch, dispatch, 2, ok),
    _Pid = moment_dispatcher:start_link(),
    0 = length(moment_dispatcher:get_queue()),
    ok = moment_dispatcher:add_moments([#moment{ next_moment=erlang:system_time(second)+1 }]),
    ok = meck:wait(1, moment_dispatcher, dispatcher, [cast, {add_moments, '_'}, '_'], 500),
    1 = length(moment_dispatcher:get_queue()),
    ok = meck:wait(1, moment_dispatch, dispatch, 2, 5000),
    0 = length(moment_dispatcher:get_queue()),
    ok = moment_dispatcher:stop().

add_one_moment_in_front_of_existing_moment(_Config) ->
    meck:expect(moments_db_mnesia, get_moments, 0,
                meck:seq([[#moment{ moment_id="1", next_moment=erlang:system_time(second)+2 }],
                          [#moment{ moment_id="1", next_moment=erlang:system_time(second)+2 }],
                          []])),
    meck:expect(moment_dispatch, dispatch, 2, ok),
    _Pid = moment_dispatcher:start_link(),
    1 = length(moment_dispatcher:get_queue()),
    ok = moment_dispatcher:add_moments([#moment{ moment_id="2", next_moment=erlang:system_time(second)+1 }]),
    ok = meck:wait(1, moment_dispatcher, dispatcher, [cast, {add_moments, '_'}, '_'], 500),
    2 = length(moment_dispatcher:get_queue()),
    ok = meck:wait(1, moment_dispatch, dispatch, [#moment{ moment_id="2", _ = '_'}, '_'], 5000),
    meck:reset(moment_dispatch),
    1 = length(moment_dispatcher:get_queue()),
    ok = meck:wait(1, moment_dispatch, dispatch, [#moment{ moment_id="1", _ = '_'}, '_'], 5000),
    0 = length(moment_dispatcher:get_queue()),
    ok = moment_dispatcher:stop().
