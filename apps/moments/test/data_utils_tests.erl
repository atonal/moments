-module(data_utils_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

is_passed_test() ->
    Moment = #moment{ next_moment=1 },
    ?assert(data_utils:is_passed(Moment, 2)).
is_passed_equal_test() ->
    Moment = #moment{ next_moment=1 },
    ?assertNot(data_utils:is_passed(Moment, 1)).
is_passed_greater_test() ->
    Moment = #moment{ next_moment=2 },
    ?assertNot(data_utils:is_passed(Moment, 1)).

is_before_test() ->
    Moment1 = #moment{ next_moment=1 },
    Moment2 = #moment{ next_moment=2 },
    ?assert(data_utils:is_before(Moment1, Moment2)).
is_before_equal_test() ->
    Moment1 = #moment{ next_moment=1 },
    Moment2 = #moment{ next_moment=1 },
    ?assertNot(data_utils:is_before(Moment1, Moment2)).
is_before_greater_test() ->
    Moment1 = #moment{ next_moment=2 },
    Moment2 = #moment{ next_moment=1 },
    ?assertNot(data_utils:is_before(Moment1, Moment2)).
