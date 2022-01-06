-module(moment_dispatcher_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

order_moments_test_() ->
    {"order moments tests", order_moments_t()}.
order_moments_t() ->
    [{"basic ordering",
      ?_assertEqual(moment_dispatcher:order_moments([#moment{ next_moment=3 },
                                   #moment{ next_moment=1 },
                                   #moment{ next_moment=2 }]),
                    [#moment{ next_moment=1 },
                     #moment{ next_moment=2 },
                     #moment{ next_moment=3 }])}].

get_next_timeout_test_() ->
    {"get next timeout tests", get_next_timeout_t()}.
get_next_timeout_t() ->
    [{"empty list",
      ?_assertEqual(infinity, moment_dispatcher:get_next_timeout([], fun(_) -> 1 end))},
    {"positive timeout",
     ?_assertEqual(2000, moment_dispatcher:get_next_timeout([#moment{ next_moment=3 }],
                                                          fun(_) -> 1 end))},
    {"negative timeout",
     ?_assertEqual(0, moment_dispatcher:get_next_timeout([#moment{ next_moment=1 }],
                                                       fun(_) -> 2 end))}].
dispatch_moments_test_() ->
    {"dispatch moments tests", dispatch_moments_t()}.
dispatch_moments_t() ->
    [{"empty list",
      ?_assertEqual([], moment_dispatcher:dispatch_moments([], 1, fun(_,_) -> ok end))},
    {"one moment dispatched",
      ?_assertEqual([], moment_dispatcher:dispatch_moments([#moment{ next_moment=1 }], 3, fun(_,_) -> ok end))},
    {"all moments dispatched",
     ?_assertEqual([], moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                         #moment{ next_moment=2 }],
                                                        3,
                                                        fun(_, _) -> ok end))},
    {"no moments dispatched",
     ?_assertEqual([#moment{ next_moment=4 },
                    #moment{ next_moment=5 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=4 },
                                                     #moment{ next_moment=5 }],
                                                    3,
                                                    fun(_, _) -> ok end))},
    {"some moments dispatched",
     ?_assertEqual([#moment{ next_moment=3 },
                    #moment{ next_moment=4 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                     #moment{ next_moment=2 },
                                                     #moment{ next_moment=3 },
                                                     #moment{ next_moment=4 }],
                                                    3,
                                                    fun(_, _) -> ok end))},
    {"function assumes ordered list, some moments dispatched",
     ?_assertEqual([#moment{ next_moment=3 },
                    #moment{ next_moment=1 },
                    #moment{ next_moment=4 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                     #moment{ next_moment=2 },
                                                     #moment{ next_moment=3 },
                                                     #moment{ next_moment=1 },
                                                     #moment{ next_moment=4 }],
                                                    3,
                                                    fun(_, _) -> ok end))}].
