%%%-------------------------------------------------------------------
%% @doc webserver public API
%% @end
%%%-------------------------------------------------------------------

-module(webserver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", base_h, []},
                                             {"/api/v1/moments", moments_h, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    webserver_sup:start_link().

stop(_State) ->
    ok.
