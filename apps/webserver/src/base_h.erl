-module(base_h).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello world!\n">>,
                           Req0),
    {ok, Req, State}.
