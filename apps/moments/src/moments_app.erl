%%%-------------------------------------------------------------------
%% @doc moments public API
%% @end
%%%-------------------------------------------------------------------

-module(moments_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    moments_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
