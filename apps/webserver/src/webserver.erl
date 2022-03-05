-module(webserver).

-export([start_link/0, stop/0]).

start_link() ->
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/", base_h, []},
                    {"/api/v1/moments", moments_h, []},
                    {"/api/v1/moments/:id", [{id, int}], moments_id_h, #{}},
                    {"/api/v1/users", users_h, []},
                    {"/api/v1/users/:id", [{id, int}], users_id_h, #{}},
                    {"/api/v1/users/:id/ws", [{id, int}], websocket_h, #{}}
                   ]}]),
    cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}).

stop() ->
    ok = cowboy:stop_listener(http).
