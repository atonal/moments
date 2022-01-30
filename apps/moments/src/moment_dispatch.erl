-module(moment_dispatch).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/2]).

-spec dispatch(moment(), integer()) -> any().
dispatch(Moment, DispatchTime) ->
    ?LOG_NOTICE("dispatching ~p", [Moment]),
    moments_db_mnesia:consume_moment(Moment#moment.moment_id, DispatchTime),
    event_hub:notify_moment(Moment),
    ok.
