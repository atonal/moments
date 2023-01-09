-module(users_id_h).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([service_available/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([to_json/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

service_available(Req, State) ->
    {mnesia:system_info(is_running) =:= yes, Req, State}.

known_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{ <<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

resource_exists(Req, State) ->
    UserId = cowboy_req:binding(id, Req),
    ?LOG_INFO("Request for user id: ~p", [UserId]),
    case moments_db_mnesia:get_user_with_links(UserId) of
        U when map_size(U) =:= 0 ->
            ?LOG_DEBUG("User not found: ~p", [UserId]),
            {false, Req, State};
        User ->
            ?LOG_DEBUG("User found: ~p", [User]),
            {true, Req, State#{data => User}}
    end.

to_json(Req, #{data := User} = State) ->
    UserMap = moments_data:users_with_links_to_jsonapi_map(User),
    Body = jsx:encode(UserMap),
    {Body, Req, State}.

is_authorized(Req, State) ->
    {Res, Req2} = moments_auth:is_authorized(Req),
    {Res, Req2, State}.
