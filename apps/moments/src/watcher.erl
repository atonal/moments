-module(watcher).
-include_lib("kernel/include/logger.hrl").

-export([init/0, start_link/0, stop/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    ?LOG_NOTICE("Subscribing to mnesia"),
    mnesia:subscribe({table, moment, simple}),
    ?LOG_NOTICE("Start watching"),
    watch().

stop() ->
    mnesia:unsubscribe({table, moment, simple}),
    ok.

watch() ->
    receive
        {mnesia_table_event, Event} ->
            case Event of
                {write, NewRecord, ActivityId} ->
                    ?LOG_NOTICE("received ~p", [[NewRecord, ActivityId]]),
                    moment_dispatcher:add_moments([NewRecord]);
                {delete_object, OldRecord, ActivityId} ->
                    ?LOG_NOTICE("received delete_object ~p", [[OldRecord, ActivityId]]);
                {delete, {Tab, Key}, ActivityId} ->
                    ?LOG_NOTICE("received delete ~p", [[{Tab, Key}, ActivityId]])
            end,
            watch();
        Unknown ->
            ?LOG_NOTICE("received unknown message: ~p", [Unknown]),
            watch()
    end.
