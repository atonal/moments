-module(users_id_h).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([service_available/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([to_json/2]).
-export([resource_exists/2]).

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
    case moments_db_mnesia:get_user(UserId) of
        {ok, User} ->
            ?LOG_DEBUG("User found: ~p", [User]),
            {true, Req, State#{data => User}};
        {error, Err} ->
            ?LOG_DEBUG("User not found: ~p, ~p", [UserId, Err]),
            {false, Req, State}
    end.

to_json(Req, #{data := User} = State) ->
    UserMap = moments_data:user_to_map(User),
    Body = jsx:encode(UserMap),
    {Body, Req, State}.
