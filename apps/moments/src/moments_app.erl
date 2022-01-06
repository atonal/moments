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
    moment_dispatcher:start_link(),
    watcher:start(),
    moments_sup:start_link().

stop(_State) ->
    ok.
