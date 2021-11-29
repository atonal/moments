-module(data_utils).
-export([is_passed/2]).

-include("data_records.hrl").

-spec is_passed(moment(), utc_time()) -> boolean().
is_passed(Moment, Time) ->
    Moment#moment.next_moment < Time.
