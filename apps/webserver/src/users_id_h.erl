-module(users_id_h).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([service_available/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

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

to_json(Req, State) ->
    UserId = cowboy_req:binding(id, Req),
    case moments_db_mnesia:get_user(UserId) of
        {ok, User} ->
            UserMap = moments_data:user_to_map(User),
            Body = jsx:encode(UserMap),
            {Body, Req, State};
        {error, _Err} ->
            Resp = cowboy_req:reply(404, Req),
            {[], Resp, State}
    end.
