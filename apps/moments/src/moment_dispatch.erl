-module(moment_dispatch).
-behavior(gen_statem).
-define(NAME, moment_dispatch).
-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

-ifdef(testing).
-export([order_moments/1, get_next_timeout/2, dispatch_moments/3]).
-endif.

-export([start_link/0, stop/0]).
-export([add_moments/1]).
-export([init/1,callback_mode/0]).
-export([dispatcher/3]).

%% API
start_link() ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?NAME).

-spec add_moments([moment()]) -> ok.
add_moments(Moments) ->
    gen_statem:cast(?NAME, {add_moments, Moments}).

%% Mandatory callback functions
init([]) ->
    Data = get_moments(10),
    Timeout = get_next_timeout(Data, fun erlang:system_time/1),
    {ok, dispatcher, Data, [{state_timeout, Timeout, check_moments}]}.

callback_mode() -> state_functions.

%% State callback functions
dispatcher(cast, {add_moments, Moments}, Data) ->
    NewList = order_moments(Data ++ Moments),
    Timeout = get_next_timeout(NewList, fun erlang:system_time/1),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]};
dispatcher(state_timeout, check_moments, Data) ->
    DispatchTime = erlang:system_time(second),
    Rest = dispatch_moments(Data, DispatchTime, fun dispatch/2),
    More = get_moments(length(Data) - length(Rest)),
    NewList = order_moments(Rest ++ More),
    Timeout = get_next_timeout(NewList, fun erlang:system_time/1),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]}.

%% Module functions
-spec get_next_timeout([moment()], fun((erlang:time_unit()) -> integer())) -> timeout() | integer(). % state_timeout() = timeout() | integer()
get_next_timeout([#moment{next_moment=Next}|_], TimeFun) ->
    Now = TimeFun(second),
    Timeout = Next - Now,
    if Timeout > 0 -> Timeout * 1000;
       Timeout =< 0 -> 0
    end;
get_next_timeout([], _) ->
    infinity.

-spec get_moments(integer()) -> [moment()].
get_moments(0) ->
    [];
get_moments(_N) ->
    % TODO: get only N moments
    moments_db_mnesia:get_moments().

-spec dispatch(moment(), integer()) -> any().
dispatch(Moment, _Time) ->
    % TODO: dispatch
    ?LOG_NOTICE("dispatching ~ts", [Moment]).

-spec dispatch_moments([moment()], integer(), fun((moment(), integer()) -> no_return())) -> [moment()].
dispatch_moments([], _, _) ->
    [];
dispatch_moments([H|T] = Moments, Time, DispatchFun) ->
    case data_utils:is_passed(H, Time) of
        true ->
            ok = DispatchFun(H, Time),
            dispatch_moments(T, Time, DispatchFun);
        false ->
            Moments
    end.

-spec order_moments([moment()]) -> [moment()].
order_moments(Moments) ->
    lists:sort(fun data_utils:is_before/2, Moments).
