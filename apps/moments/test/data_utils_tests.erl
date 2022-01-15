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

time_add_test_() ->
    {"time_add tests", time_add()}.
time_add() ->
    [{"second", ?_assertEqual(2, data_utils:time_add(1, {second, 1}))},
     {"seconds", ?_assertEqual(3, data_utils:time_add(1, {second, 2}))},
     {"minute", ?_assertEqual(61, data_utils:time_add(1, {minute, 1}))},
     {"minutes", ?_assertEqual(181, data_utils:time_add(1, {minute, 3}))},
     {"hour", ?_assertEqual(3601, data_utils:time_add(1, {hour, 1}))},
     {"day", ?_assertEqual(86_401, data_utils:time_add(1, {day, 1}))},
     {"week", ?_assertEqual(604_801, data_utils:time_add(1, {week, 1}))},
     {"month", ?_assertEqual(2_592_001, data_utils:time_add(1, {month, 1}))},
     {"year", ?_assertEqual(31_536_001, data_utils:time_add(1, {year, 1}))}].

get_next_moment_test_() ->
    {"get_next_moment tests", get_next_moment()}.
get_next_moment() ->
    [
     {"debug", ?_assertEqual(61, data_utils:get_next_moment(#moment{ next_moment=1, interval=debug }))},
     {"hourly", ?_assertEqual(3601, data_utils:get_next_moment(#moment{ next_moment=1, interval=hourly }))},
     {"daily", ?_assertEqual(86_401, data_utils:get_next_moment(#moment{ next_moment=1, interval=daily }))},
     {"weekly", ?_assertEqual(604_801, data_utils:get_next_moment(#moment{ next_moment=1, interval=weekly }))},
     {"monthly", ?_assertEqual(2_592_001, data_utils:get_next_moment(#moment{ next_moment=1, interval=monthly }))},
     {"yearly", ?_assertEqual(31_536_001, data_utils:get_next_moment(#moment{ next_moment=1, interval=yearly }))}
    ].
