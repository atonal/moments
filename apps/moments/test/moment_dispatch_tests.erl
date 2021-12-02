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
