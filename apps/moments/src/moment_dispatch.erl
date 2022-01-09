-module(moment_dispatch).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/1]).

-spec dispatch(moment()) -> any().
dispatch(Moment) ->
    % TODO: dispatch
    ?LOG_NOTICE("dispatching ~p", [Moment]),
    ok.
