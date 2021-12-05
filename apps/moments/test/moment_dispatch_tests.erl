-module(moment_dispatch_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

order_moments_test_() ->
    {"order moments tests", order_moments_t()}.
order_moments_t() ->
    [{"basic ordering",
      ?_assertEqual(moment_dispatch:order_moments([#moment{ next_moment=3 },
                                   #moment{ next_moment=1 },
                                   #moment{ next_moment=2 }]),
                    [#moment{ next_moment=1 },
                     #moment{ next_moment=2 },
                     #moment{ next_moment=3 }])}].

get_next_timeout_test_() ->
    {"get next timeout tests", get_next_timeout_t()}.
get_next_timeout_t() ->
    [{"empty list",
      ?_assertEqual(infinity, moment_dispatch:get_next_timeout([], fun(_) -> 1 end))},
    {"positive timeout",
     ?_assertEqual(2000, moment_dispatch:get_next_timeout([#moment{ next_moment=3 }],
                                                          fun(_) -> 1 end))},
    {"negative timeout",
     ?_assertEqual(0, moment_dispatch:get_next_timeout([#moment{ next_moment=1 }],
                                                       fun(_) -> 2 end))}].
