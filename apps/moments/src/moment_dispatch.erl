-module(moment_dispatch).
-export([start/0, init/0]).
-import(moments_db_mnesia, [get_moments/0]).
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("data_records.hrl").

start() ->
    spawn(?MODULE, init, []).

init() ->
    Moments = get_first_moments(),
    loop(Moments, []).

-spec get_first_moments() -> [moment()].
get_first_moments() ->
    get_moments().

-spec is_due(moment()) -> boolean().
is_due(Moment) ->
    data_utils:is_passed(Moment, erlang:system_time(second)).

-spec dispatch(moment()) -> no_return().
dispatch(Moment) ->
    % TODO: dispatch
    ?LOG_NOTICE("dispatching ~ts", [Moment]).

-spec dispatch_moments([moment()]) -> [moment()].
dispatch_moments([]) ->
    [];
dispatch_moments([H|T] = Moments) ->
    case is_due(H) of
        true ->
            ok = dispatch(H),
            dispatch_moments(T);
        false ->
            Moments
    end.

-spec order_moments([moment()]) -> [moment()].
order_moments(Moments) ->
    lists:sort(fun data_utils:is_before/2, Moments).

-spec loop([moment()], [reference()]) -> [moment()].
loop(Moments, Timers) ->
    receive
        check_moments ->
            Rest = dispatch_moments(Moments),
            % TODO: get timeout from first
            NewTimer = erlang:send_after(1000, self(), check_moments),
            loop(Rest, [NewTimer]);
        {add_moments, New} ->
            NewList = order_moments(Moments ++ New),
            ok = lists:foreach(
              fun(T) -> erlang:cancel_timer(T, [{async, true}, {info, false}]) end,
              Timers),
            % TODO: get timeout from first
            NewTimer = erlang:send_after(1000, self(), check_moments),
            loop(NewList, [NewTimer]);
        _ ->
            loop(Moments, Timers)
    end.

%% eunit
order_moments_test_() ->
    {"order moments tests", order_moments_t()}.
order_moments_t() ->
    [{"basic ordering",
      ?_assertEqual(order_moments([#moment{ next_moment=3 },
                                   #moment{ next_moment=1 },
                                   #moment{ next_moment=2 }]),
                    [#moment{ next_moment=1 },
                     #moment{ next_moment=2 },
                     #moment{ next_moment=3 }])}].
