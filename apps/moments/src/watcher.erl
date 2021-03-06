-module(watcher).
-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

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
                {write, NewRecord, ActivityId} when is_record(NewRecord, moment) ->
                    ?LOG_NOTICE("received: ~p", [[NewRecord, ActivityId]]),
                    moment_dispatcher:add_moments([NewRecord]);
                {write, NewRecord, ActivityId} ->
                    ?LOG_DEBUG("received, ignore: ~p", [[NewRecord, ActivityId]]);
                {delete_object, OldRecord, ActivityId} when is_record(OldRecord, moment) ->
                    ?LOG_NOTICE("received delete_object: ~p", [[OldRecord, ActivityId]]),
                    % Erlang doc says "A record has _possibly_ been deleted", so check if it so
                    case moments_db_mnesia:get_moment(OldRecord#moment.moment_id) of
                        {ok, M} ->
                            ?LOG_NOTICE("Object still exists, don't remove! OldRecord: ~p, Moment from DB: ~p", [OldRecord, M]);
                        {error, moment_doesnt_exists} ->
                            moment_dispatcher:remove_moments([OldRecord])
                    end;
                {delete_object, OldRecord, ActivityId} ->
                    ?LOG_DEBUG("received delete_object, ignore: ~p", [[OldRecord, ActivityId]]);
                {delete, {moment, Key}, ActivityId} ->
                    ?LOG_NOTICE("received delete: ~p", [[{moment, Key}, ActivityId]]),
                    % Erlang doc says "A record has _possibly_ been deleted", so check if it so
                    case moments_db_mnesia:get_moment(Key) of
                        {ok, M} ->
                            ?LOG_NOTICE("Object still exists, on't remove! Key: ~p, Moment from DB: ~p", [Key, M]);
                        {error, moment_doesnt_exists} ->
                            moment_dispatcher:remove_moments([Key])
                    end;
                {delete, {Tab, Key}, ActivityId} ->
                    ?LOG_DEBUG("received delete, ignore: ~p", [[{Tab, Key}, ActivityId]])
            end,
            watch();
        Unknown ->
            ?LOG_NOTICE("received unknown message: ~p", [Unknown]),
            watch()
    end.
