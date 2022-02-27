-module(moments_id_h).

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
    MomentId = cowboy_req:binding(id, Req),
    case moments_db_mnesia:get_moment_with_links(MomentId) of
        M when map_size(M) =:= 0 ->
            ?LOG_INFO("moments_id_h got empty map"),
            Resp = cowboy_req:reply(404,
                                    #{<<"content-type">> => <<"application/json">>},
                                    jsx:encode(#{error=>
                                                 #{reason=> <<"Moment not found">>}}),
                                    Req),
            {[], Resp, State};
        Moment ->
            ?LOG_INFO("moments_id_h got map: ~p", [Moment]),
            MomentMap = moments_data:moments_with_links_to_jsonapi_map(Moment),
            Body = jsx:encode(MomentMap),
            {Body, Req, State}
    end.
