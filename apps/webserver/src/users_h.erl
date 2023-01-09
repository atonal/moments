-module(users_h).

-include("../../moments/src/data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([service_available/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([from_json/2]).
-export([is_authorized/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

service_available(Req, State) ->
    {mnesia:system_info(is_running) =:= yes, Req, State}.

known_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{ <<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{ <<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

to_json(Req, State) ->
    Users = moments_db_mnesia:get_users_with_links(),
    UserMaps = moments_data:users_with_links_to_jsonapi_map(Users),
    Body = jsx:encode(UserMaps),
    {Body, Req, State}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

from_json(Req0, State) ->
    {ok, ReqBody, Req} = read_body(Req0, <<>>),
    ?LOG_DEBUG("users_h: ReqBody: ~p", [ReqBody]),
    ReqUser = jsx:decode(ReqBody),
    Result = case moments_data:parse_user_map(ReqUser) of
                 User when is_record(User, user) ->
                     case moments_db_mnesia:insert_user(User) of
                         Uid when is_integer(Uid) ->
                             {see_other, <<"/api/v1/users/", (integer_to_binary(Uid))/binary>>};
                         {error, Err} ->
                             cowboy_req:reply(
                               500,
                               #{<<"content-type">> => <<"application/json">>},
                               jsx:encode(#{error=>
                                            #{reason=> <<"server error">>,
                                              text => Err}}),
                               Req)
                     end;
                 {error, Err} ->
                     cowboy_req:reply(
                       400,
                       #{<<"content-type">> => <<"application/json">>},
                       jsx:encode(#{error=>
                                    #{reason=> <<"malformed body">>,
                                      text => Err}}),
                       Req)
             end,
    {Result, Req, State}.

is_authorized(Req, State) ->
    {Res, Req2} = moments_auth:is_authorized(Req),
    {Res, Req2, State}.
