-module(webserver).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0, stop/0]).

start_link() ->
    ConfigEndpoint = <<"http://localhost:9090/realms/moments/.well-known/openid-configuration">>,
    LocalEndpoint = <<"http://localhost:8080/oidc">>,
    {ok, ClientId} = application:get_env(oidc_client_id),
    {ok, ClientSecret} = application:get_env(oidc_client_secret),
    ?LOG_NOTICE("oidc_client_id: ~p", [ClientId]),
    ?LOG_NOTICE("oidc_client_secret: ~p", [ClientSecret]),
    Config = #{
               id => <<"moments">>,
               client_id => list_to_binary(ClientId),
               client_secret => list_to_binary(ClientSecret)
              },
    {ok, OidcId, Pid} = oidcc:add_openid_provider(ConfigEndpoint, LocalEndpoint, Config),
    ?LOG_NOTICE("Added OIDC provider: ~p", [OidcId]),
    ok = wait_for_config(Pid),
    moments_oidcc:init(),

    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/", base_h, []},
                    {"/login", login_h, []},
                    {"/oidc", oidcc_cowboy, []},
                    {"/api/v1/moments", moments_h, []},
                    {"/api/v1/moments/:id", [{id, int}], moments_id_h, #{}},
                    {"/api/v1/users", users_h, []},
                    {"/api/v1/users/:id", [{id, int}], users_id_h, #{}},
                    {"/api/v1/users/:id/ws", [{id, int}], websocket_h, #{}}
                   ]}]),
    cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}).

stop() ->
    ok = cowboy:stop_listener(http).

wait_for_config(Pid) ->
    Ready = oidcc_openid_provider:is_ready(Pid),
    {ok, Error} = oidcc_openid_provider:get_error(Pid),
    case {Ready, Error}  of
        {true, undefined} ->
            ok;
        {false, undefined} ->
            timer:sleep(100),
            wait_for_config(Pid);
        _ ->
            {error, Error}
    end.
