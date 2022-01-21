-module(watcher_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/data_records.hrl").

-compile(export_all).

all() -> [write_event,
          delete_object_event,
          delete_object_event_object_still_exists,
          delete_event,
          delete_event_still_exists].

init_per_testcase(_, Config) ->
    meck:new(moment_dispatcher),
    meck:new(moments_db_mnesia),
    meck:expect(mnesia, subscribe, 1, {ok, node()}),
    {ok, Pid} = watcher:start_link(),
    ok = meck:wait(1, mnesia, subscribe, [{table, moment, simple}], 5000),
    [{pid, Pid} | Config].

end_per_testcase(_, _Config) ->
    true = meck:validate(moments_db_mnesia),
    meck:unload(moments_db_mnesia),
    true = meck:validate(moment_dispatcher),
    meck:unload(moment_dispatcher),
    ok.

init_per_suite(Config) ->
    meck:new(mnesia, [no_link]),
    Config.

end_per_suite(_Config) ->
    true = meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

% Tests
write_event(Config) ->
    meck:expect(moment_dispatcher, add_moments, 1, ok),
    WatcherPid = ?config(pid, Config),
    Event = {write, db_obj, activity_id},
    WatcherPid ! {mnesia_table_event, Event},
    ok = meck:wait(1, moment_dispatcher, add_moments, [[db_obj]], 5000).

delete_object_event(Config) ->
    Mid = 1,
    Moment = #moment{moment_id=Mid},
    meck:expect(moment_dispatcher, remove_moments, 1, ok),
    meck:expect(moments_db_mnesia, get_moment, 1, {error, moment_doesnt_exists}),
    WatcherPid = ?config(pid, Config),
    Event = {delete_object, Moment, activity_id},
    WatcherPid ! {mnesia_table_event, Event},
    ok = meck:wait(1, moments_db_mnesia, get_moment, [Mid], 5000),
    ok = meck:wait(1, moment_dispatcher, remove_moments, [[Moment]], 5000).

delete_object_event_object_still_exists(Config) ->
    Mid = 1,
    Moment = #moment{moment_id=Mid},
    meck:expect(moment_dispatcher, remove_moments, 1, ok),
    meck:expect(moments_db_mnesia, get_moment, 1, {ok, Mid}),
    WatcherPid = ?config(pid, Config),
    Event = {delete_object, Moment, activity_id},
    WatcherPid ! {mnesia_table_event, Event},
    ok = meck:wait(1, moments_db_mnesia, get_moment, [Mid], 5000).

delete_event(Config) ->
    Mid = 1,
    meck:expect(moment_dispatcher, remove_moments, 1, ok),
    meck:expect(moments_db_mnesia, get_moment, 1, {error, moment_doesnt_exists}),
    WatcherPid = ?config(pid, Config),
    Event = {delete, {moment, Mid}, activity_id},
    WatcherPid ! {mnesia_table_event, Event},
    ok = meck:wait(1, moments_db_mnesia, get_moment, [Mid], 5000),
    ok = meck:wait(1, moment_dispatcher, remove_moments, [[Mid]], 5000).

delete_event_still_exists(Config) ->
    Mid = 1,
    meck:expect(moment_dispatcher, remove_moments, 1, ok),
    meck:expect(moments_db_mnesia, get_moment, 1, {ok, Mid}),
    WatcherPid = ?config(pid, Config),
    Event = {delete, {moment, Mid}, activity_id},
    WatcherPid ! {mnesia_table_event, Event},
    ok = meck:wait(1, moments_db_mnesia, get_moment, [Mid], 5000).
