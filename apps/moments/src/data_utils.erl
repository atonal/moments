-module(data_utils).
-export([is_passed/2, is_before/2]).

-include("data_records.hrl").

-spec is_passed(moment(), utc_time()) -> boolean().
is_passed(Moment, Time) ->
    Moment#moment.next_moment =< Time.

-spec is_before(moment(), moment()) -> boolean().
is_before(Moment1, Moment2) ->
    Moment1#moment.next_moment < Moment2#moment.next_moment.
