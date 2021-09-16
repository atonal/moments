%%%-------------------------------------------------------------------
%% @doc moments public API
%% @end
%%%-------------------------------------------------------------------

-module(moments_app).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).
-export([init/1]).

start(_StartType, _StartArgs) ->
    init([node()]),
    moments_sup:start_link().

stop(_State) ->
    ok.

create_tables(Nodes) ->
    {atomic, ok} = mnesia:create_table(moment, [{attributes, record_info(fields, moment)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(device, [{attributes, record_info(fields, device)},
                                 {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(follows, [{attributes, record_info(fields, follows)},
                                 {disc_copies, Nodes},
                                 {index, [#follows.moment]},
                                 {type, bag}]),
    {atomic, ok} = mnesia:create_table(admin_of, [{attributes, record_info(fields, admin_of)},
                                 {disc_copies, Nodes},
                                 {index, [#admin_of.moment]},
                                 {type, bag}]),
    {atomic, ok} = mnesia:create_table(owns, [{attributes, record_info(fields, owns)},
                                 {disc_copies, Nodes},
                                 {index, [#owns.device]},
                                 {type, bag}]).

init(Nodes) ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {aborted, {already_exists, schema, Node, disc_copies}} ->
            ?LOG_NOTICE("exists ~ts", [Node]);
        {atomic, ok} ->
            ?LOG_NOTICE("schema copy type changed OK"),
            create_tables(Nodes)
    end.
