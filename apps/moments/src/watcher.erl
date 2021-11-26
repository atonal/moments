-module(watcher).
-include_lib("kernel/include/logger.hrl").

-export([start/0, stop/0]).

start() ->
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
                    ?LOG_NOTICE("received ~p", [[NewRecord, ActivityId]]);
                {delete_object, OldRecord, ActivityId} ->
                    ?LOG_NOTICE("received ~p", [[OldRecord, ActivityId]]);
                {delete, {Tab, Key}, ActivityId} ->
                    ?LOG_NOTICE("received ~p", [[{Tab, Key}, ActivityId]])
            end,
            watch();
        Unknown ->
            ?LOG_NOTICE("received unknown message: ~p", [Unknown]),
            watch()
    end.
