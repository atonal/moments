%%%-------------------------------------------------------------------
%% @doc webserver public API
%% @end
%%%-------------------------------------------------------------------

-module(webserver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    webserver_sup:start_link().

stop(_State) ->
    ok.
