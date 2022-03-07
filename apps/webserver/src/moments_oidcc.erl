-module(moments_oidcc).
-behaviour(oidcc_client).

-include_lib("kernel/include/logger.hrl").

-export([init/0]).
-export([login_succeeded/1]).
-export([login_succeeded/2]).
-export([login_failed/2]).
-export([login_failed/3]).

init() ->
    oidcc_client:register(?MODULE).

login_succeeded(Token) ->
    login_succeeded(Token, #{}).

login_succeeded(Token, EnvMap) ->
    ?LOG_INFO("~n~n*************************************~nthe user logged in with~n ~p~n", [Token]),
    ?LOG_INFO("Env: ~p~n~n***************************************~n", [EnvMap]),
    CookieName = login_h:cookie_name(),
    Path = <<"/">>,
    Updates = [
               {redirect, Path},
               {cookie,
                CookieName,
                maps:get(token, maps:get(access, Token)),
                #{max_age => 30, http_only => true}},
               {cookie,
                <<"refresh_token">>,
                maps:get(token, maps:get(refresh, Token)),
                #{max_age => 30, http_only => true}},
               {cookie,
                <<"id_token">>,
                maps:get(token, maps:get(id, Token)),
                #{max_age => 30, http_only => true}}
              ],
    {ok, Updates}.


login_failed(Error, Desc) ->
    login_failed(Error, Desc, #{}).

login_failed(Error, Desc, EnvMap) ->
    ?LOG_INFO("~n~n*************************************~nlogin failed with~n ~p:~p~n", [Error, Desc]),
    ?LOG_INFO("Env: ~p~n~n***************************************~n", [EnvMap]),
    Path = <<"/">>,
    Updates = [{redirect, Path}],
    {ok, Updates}.
