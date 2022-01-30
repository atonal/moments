-module(moment_h).

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
    case moments_db_mnesia:get_moment(MomentId) of
        {ok, Moment} ->
            MomentMap = moments_data:moment_to_map(Moment),
            Body = jsx:encode(MomentMap),
            {Body, Req, State};
        {error, Err} ->
            % TODO: return 404
            {atom_to_list(Err), Req, State}
    end.
