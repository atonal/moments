-module(moment_dispatch).
-behavior(gen_statem).
-define(NAME, moment_dispatch).
-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

-ifdef(testing).
-export([order_moments/1]).
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
    Timeout = get_next_timeout(Data),
    {ok, dispatcher, Data, [{state_timeout, Timeout, check_moments}]}.

callback_mode() -> state_functions.

%% State callback functions
dispatcher(cast, {add_moments, Moments}, Data) ->
    NewList = order_moments(Data ++ Moments),
    Timeout = get_next_timeout(NewList),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]};
dispatcher(state_timeout, check_moments, Data) ->
    Rest = dispatch_moments(Data),
    More = get_moments(length(Data) - length(Rest)),
    NewList = order_moments(Rest ++ More),
    Timeout = get_next_timeout(NewList),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]}.

%% Module functions
-spec get_next_timeout([moment()]) -> integer().
get_next_timeout(_) ->
    % TODO: implement
    10000.

-spec get_moments(integer()) -> [moment()].
get_moments(0) ->
    [];
get_moments(_N) ->
    % TODO: get only N moments
    moments_db_mnesia:get_moments().

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
