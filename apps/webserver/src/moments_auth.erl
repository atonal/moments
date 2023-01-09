-module(moments_auth).

-include_lib("kernel/include/logger.hrl").

-export([is_authenticated/1]).
-export([is_authorized/1]).

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

auth_checks() ->
    [
     fun check_bearer/1,
     fun check_session/1
    ].

-spec is_authorized(cowboy_req:req()) -> {Res, cowboy_req:req()} when
      Res :: true | {false, bitstring()}.
is_authorized(Req) ->
    is_authorized(Req, auth_checks()).
is_authorized(Req, Funcs) ->
    is_authorized(Req, Funcs, []).

-spec is_authorized(cowboy_req:req(), list(fun()), list(Res)) -> {Res, cowboy_req:req()} when
      Res :: true | {false, bitstring()}.
is_authorized(Req, _, [true|_]) ->
    {true, Req};
is_authorized(Req, [], [Res|_]) ->
    {Res, Req}; % TODO: return a list of methods
is_authorized(Req, [CheckFun|T], Acc) ->
    {Res, Req2} = CheckFun(Req),
    is_authorized(Req2, T, [Res] ++ Acc).

-spec check_session(cowboy_req:req()) -> {Res, cowboy_req:req()} when
      Res :: true | {false, bitstring()}.
check_session(Req) ->
    ?LOG_DEBUG("moments_h: check_session"),
    {User, Req2} = cowboy_session:get(user, Req),
    ?LOG_DEBUG("moments_h: User: ~p~n",[User]),
    case User of
        undefined ->
            {{false, <<"Cookie cookie-name=session">>}, Req2};
        _ ->
            % TODO: Check that local user exists
            {true, Req2}
    end.

-spec check_bearer(cowboy_req:req()) -> {Res, cowboy_req:req()} when
      Res :: true | {false, bitstring()}.
check_bearer(Req) ->
    ?LOG_DEBUG("moments_h: check_bearer"),
    Tok = parse_token(extract_token(Req)),
    Res = check_token(Tok),
    {Res, Req}.

extract_token(Req) ->
    cowboy_req:header(<<"authorization">>, Req).

parse_token(<<"Bearer ", Tok/binary>>) ->
    Tok;
parse_token(Tok) ->
    ?LOG_DEBUG("moments_h: token undefined: ~p", [Tok]),
    undefined.

check_token(undefined) ->
    ?LOG_DEBUG("moments_h: no token"),
    {false, <<"Bearer">>};
check_token(Token) ->
    ?LOG_DEBUG("moments_h: check token"),
    IsAuth = is_authenticated(Token),
    auth_result(IsAuth).

auth_result({ok, Tok}) ->
    ?LOG_DEBUG("moments_h: is authenticated! ~p", [Tok]),
    true;
auth_result({error, Tok}) ->
    ?LOG_DEBUG("moments_h: not authenticated! ~p", [Tok]),
    {false, <<"Bearer">>}.
