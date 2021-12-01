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
