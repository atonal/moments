-module(moment_dispatcher).
-behavior(gen_statem).
-define(NAME, moment_dispatcher).
-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

-ifdef(testing).
-export([get_next_timeout/2, dispatch_moments/3]).
-endif.

-export([start_link/0, stop/0]).
-export([add_moments/1, get_queue/0]).
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

-spec get_queue() -> [moment()].
get_queue() ->
    gen_statem:call(?NAME, get_queue, 5000).

%% Mandatory callback functions
init([]) ->
    ?LOG_DEBUG("~p init", [?NAME]),
    Data = orddict_from_moment_list(get_moments(10)),
    Timeout = get_next_timeout(Data, fun erlang:system_time/1),
    {ok, dispatcher, Data, [{state_timeout, Timeout, check_moments}]}.

callback_mode() -> state_functions.

%% State callback functions
dispatcher(cast, {add_moments, Moments}, Data) ->
    ?LOG_INFO("add moments: ~p", [Moments]),
    NewList = moments_orddict:store_list(moment_list_to_dict_list(Moments), Data),
    Timeout = get_next_timeout(NewList, fun erlang:system_time/1),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]};
dispatcher({call, From}, get_queue, Data) ->
    ?LOG_DEBUG("get queue: ~p", [Data]),
    {keep_state_and_data, [{reply,From,Data}]};
dispatcher(state_timeout, check_moments, Data) ->
    ?LOG_INFO("state_timeout: ~p", [Data]),
    DispatchTime = erlang:system_time(second),
    _Rest = dispatch_moments(Data, DispatchTime, fun moment_dispatch:dispatch/1),
    NewList = orddict_from_moment_list(get_moments(10)),
    Timeout = get_next_timeout(NewList, fun erlang:system_time/1),
    {keep_state, NewList, [{state_timeout, Timeout, check_moments}]}.

%% Module functions
-spec orddict_from_moment_list([moment()]) -> moments_orddict:orddict().
orddict_from_moment_list(List) ->
    moments_orddict:from_list(moment_list_to_dict_list(List)).

-spec moment_list_to_dict_list([moment()]) -> moments_orddict:orddict(moment_id(), moment()).
moment_list_to_dict_list(Moments) ->
    [{X#moment.moment_id, X} || X <- Moments].

-spec get_next_timeout(moments_orddict:orddict(), TimeFun) -> StateTimeout when
      StateTimeout :: timeout() | integer(), % state_timeout from gen_statem
      TimeFun :: fun((erlang:time_unit()) -> integer()).
get_next_timeout([{_, #moment{next_moment=Next}}|_], TimeFun) ->
    Now = TimeFun(second),
    Timeout = Next - Now,
    ?LOG_INFO("Now: ~p, timeout: ~p", [Now, Next]),
    if Timeout > 0 -> Timeout * 1000;
       Timeout =< 0 -> 0
    end;
get_next_timeout([], _) ->
    infinity.

-spec get_moments(integer()) -> [moment()].
get_moments(_N) ->
    % TODO: get only N moments
    moments_db_mnesia:get_moments().

-spec dispatch_moments(moments_orddict:orddict(), integer(), DispFun) -> [moment()] when
      DispFun :: fun((moment()) -> any()).
dispatch_moments([], _, _) ->
    [];
dispatch_moments([{_,Moment}|T] = Moments, Time, DispatchFun) ->
    case data_utils:is_passed(Moment, Time) of
        true ->
            ?LOG_INFO("dispatching: ~p", [Moment]),
            ok = DispatchFun(Moment),
            dispatch_moments(T, Time, DispatchFun);
        false ->
            Moments
    end.
