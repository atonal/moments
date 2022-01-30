%%%-------------------------------------------------------------------
%% @doc moments public API
%% @end
%%%-------------------------------------------------------------------

-module(moments_app).

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    moments_db_mnesia:init([node()]),
    R = {ok, _Pid} = moments_sup:start_link(),
    ok = event_hub:log_handler(on),
    R.

stop(_State) ->
    ok.
