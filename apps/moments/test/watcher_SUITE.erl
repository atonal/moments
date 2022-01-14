-module(watcher_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [write_event].

init_per_testcase(_, Config) ->
    meck:new(moment_dispatcher),
    meck:expect(mnesia, subscribe, 1, {ok, node()}),
    {ok, Pid} = watcher:start_link(),
    ok = meck:wait(1, mnesia, subscribe, [{table, moment, simple}], 5000),
    [{pid, Pid} | Config].

end_per_testcase(_, _Config) ->
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
