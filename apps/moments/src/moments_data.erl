-module(moments_data).

-include("data_records.hrl").

-export([moment_to_map/1]).
-export([user_to_map/1]).
-export([device_to_map/1]).

-define(to_map(Tag),
        Fields = record_info(fields, Tag),
        [_Tag | Values] = tuple_to_list(M),
        L = lists:zip(Fields, Values),
        maps:from_list(L)).

moment_to_map(M) -> ?to_map(moment).
user_to_map(M) -> ?to_map(user).
device_to_map(M) -> ?to_map(device).
