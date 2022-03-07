-module(moments_auth).

-include_lib("kernel/include/logger.hrl").

-export([is_authenticated/1]).

-define(PROVIDER, <<"moments">>).

is_authenticated(undefined) ->
    ?LOG_DEBUG("moments_auth: no token"),
    {error, no_token};
is_authenticated(Token) ->
    ?LOG_DEBUG("moments_auth: introspect token: ~p", [Token]),
    Introspection = oidcc:introspect_token(Token, ?PROVIDER),
    ?LOG_DEBUG("moments_auth: introspection: ~p", [Introspection]),
    check_introspection(Introspection).

check_introspection({ok, #{active := false} = Map}) ->
    {error, Map};
check_introspection({ok, #{active := true} = Map}) ->
    {ok, Map}.
