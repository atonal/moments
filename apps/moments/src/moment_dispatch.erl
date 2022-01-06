-module(moment_dispatch).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/2]).

-spec dispatch(moment(), integer()) -> any().
dispatch(Moment, _Time) ->
    % TODO: dispatch
    ?LOG_NOTICE("dispatching ~ts", [Moment]).
