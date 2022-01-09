-module(moment_dispatcher_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    meck:new(mock, [non_strict]),
    meck:expect(mock, dispatch, 1, ok).

stop(_) ->
    meck:unload(mock).

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
    {"dispatch moments tests",
     [{"empty list",
       ?setup(fun empty_list/1)},
      {"one moment dispatched",
       ?setup(fun one_moment_dispatched/1)},
      {"all moments dispatched",
       ?setup(fun all_moments_dispatched/1)},
      {"no moments dispatched",
       ?setup(fun no_moments_dispatched/1)},
      {"some moments dispatched",
       ?setup(fun some_moments_dispatched/1)},
      {"function assumes ordered list, some moments dispatched",
       ?setup(fun unordered_list_some_moments_dispatched/1)}]}.

empty_list(_) ->
    [?_assertEqual([], moment_dispatcher:dispatch_moments([], 1, fun mock:dispatch/1)),
     ?_assertEqual(0, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:validate(mock))].

one_moment_dispatched(_) ->
    [?_assertEqual([], moment_dispatcher:dispatch_moments([#moment{ next_moment=1 }],
                                                          3,
                                                          fun mock:dispatch/1)),
     ?_assertEqual(1, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:validate(mock))].

all_moments_dispatched(_) ->
    [?_assertEqual([], moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                           #moment{ next_moment=2 }],
                                                          3,
                                                          fun mock:dispatch/1)),
     ?_assertEqual(2, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=1 }])),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=2 }])),
     ?_assert(meck:validate(mock))].

no_moments_dispatched(_) ->
    [?_assertEqual([#moment{ next_moment=4 },
                    #moment{ next_moment=5 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=4 },
                                                       #moment{ next_moment=5 }],
                                                      3,
                                                      fun mock:dispatch/1)),
     ?_assertEqual(0, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:validate(mock))].

some_moments_dispatched(_) ->
    [?_assertEqual([#moment{ next_moment=3 },
                    #moment{ next_moment=4 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                       #moment{ next_moment=2 },
                                                       #moment{ next_moment=3 },
                                                       #moment{ next_moment=4 }],
                                                      2,
                                                      fun mock:dispatch/1)),
     ?_assertEqual(2, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=1 }])),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=2 }])),
     ?_assert(meck:validate(mock))].

unordered_list_some_moments_dispatched(_) ->
    [?_assertEqual([#moment{ next_moment=3 },
                    #moment{ next_moment=1 },
                    #moment{ next_moment=4 }],
                   moment_dispatcher:dispatch_moments([#moment{ next_moment=1 },
                                                       #moment{ next_moment=2 },
                                                       #moment{ next_moment=3 },
                                                       #moment{ next_moment=1 },
                                                       #moment{ next_moment=4 }],
                                                      2,
                                                      fun mock:dispatch/1)),
     ?_assertEqual(2, meck:num_calls(mock, dispatch, '_')),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=1 }])),
     ?_assert(meck:called(mock, dispatch, [#moment{ next_moment=2 }])),
     ?_assert(meck:validate(mock))].
