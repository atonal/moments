-module(moments_h).

-include("../../moments/src/data_records.hrl").
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
    Moments = moments_db_mnesia:get_moments(),
    MomentMaps = lists:map(fun moment_to_map/1, Moments),
    Body = jsx:encode(MomentMaps),
	{Body, Req, State}.

moment_to_map(M) ->
    Fields = record_info(fields, moment),
    [_Tag | Values] = tuple_to_list(M),
    L = lists:zip(Fields, Values),
    maps:from_list(L).
