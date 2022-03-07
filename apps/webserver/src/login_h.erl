-module(login_h).

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).
-export([cookie_name/0]).

-define(COOKIE, <<"access_token">>).

-record(state, {
          access_token = undefined
         }).


cookie_name() ->
    ?COOKIE.

init(Req, _Opts) ->
    try extract_args(Req) of
        {ok, Req2, State} ->
            handle(Req2, State)
    catch
        _:_ ->
            handle(Req, #state{})
    end.

handle(Req, #state{access_token = Token } = State) ->
    %% clear the cookie again, so after a page reload one can retest it.
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<>>, Req, #{max_age => 0, http_only => true, path => <<"/">>}),
    Req3 = cowboy_req:reply(200, #{}, get_body(moments_auth:is_authenticated(Token)), Req2),
    {ok, Req3, State}.

get_body({ok, _}) ->
"<!DOCTYPE html>
<html lang=\"en\">
    <body>
	   you are logged in
    </body>
</html>
";
get_body(_) ->
" <!DOCTYPE html>
<html lang=\"en\">
    <body>
	   login
	   <a href=\"/oidc?provider=moments&use_cookie=true\">with using a cookie</a>
           </br>
    </body>
</html>
".


terminate(_Reason, _Req, _State) ->
    ok.

extract_args(Req) ->
    C = list_to_atom(binary_to_list(?COOKIE)),
    #{C := Token} = cowboy_req:match_cookies([C], Req),
    NewState = #state{access_token = Token},
    {ok, Req, NewState}.
