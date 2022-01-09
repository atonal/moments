-module(data_utils_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

is_passed_test_() ->
    {"is_passed tests", is_passed()}.
is_passed() ->
    [{"passed", ?_assert(data_utils:is_passed(#moment{ next_moment=1 }, 2))},
     {"equals", ?_assert(data_utils:is_passed(#moment{ next_moment=1 }, 1))},
     {"not passed", ?_assertNot(data_utils:is_passed(#moment{ next_moment=2 }, 1))}].

is_before_test_() ->
    {"is before tests", is_before()}.
is_before() ->
    [{"before", ?_assert(data_utils:is_before(#moment{ next_moment=1 }, #moment{ next_moment=2 }))},
     {"equals", ?_assertNot(data_utils:is_before(#moment{ next_moment=1 }, #moment{ next_moment=1 }))},
     {"after", ?_assertNot(data_utils:is_before(#moment{ next_moment=2 }, #moment{ next_moment=1 }))}].
