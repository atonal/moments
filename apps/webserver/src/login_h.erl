-module(login_h).

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).
-export([cookie_name/0]).

% "session" defined (hard-coded) in cowboy_session_config in forks under dronowar
-define(COOKIE, <<"session">>).

-record(state, {
          session = undefined
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

handle(Req, #state{session = Token } = State) ->
    %% clear the cookie again, so after a page reload one can retest it.
    % Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<>>, Req, #{max_age => 0, http_only => true, path => <<"/">>}),
    {User, Req2} = cowboy_session:get(user, Req),
    Req3 = cowboy_req:reply(200, #{}, get_body(User), Req2),
    {ok, Req3, State}.

get_body(undefined) ->
" <!DOCTYPE html>
<html lang=\"en\">
    <body>
	   login
	   <a href=\"/oidc?provider=moments&use_cookie=true\">with using a cookie</a>
           </br>
    </body>
</html>
";
get_body(User) ->
"<!DOCTYPE html>
<html lang=\"en\">
    <body>
	   you are logged in, " ++ binary_to_list(User) ++ "
    </body>
</html>
".


terminate(_Reason, _Req, _State) ->
    ok.

extract_args(Req) ->
    C = list_to_atom(binary_to_list(?COOKIE)),
    #{C := Token} = cowboy_req:match_cookies([C], Req),
    NewState = #state{session = Token},
    {ok, Req, NewState}.
