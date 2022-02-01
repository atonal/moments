-module(moments_data).

-include("data_records.hrl").

-export([moment_to_map/1]).
-export([user_to_map/1]).
-export([device_to_map/1]).
-export([map_to_moment/1]).

-define(to_map(Tag),
        Fields = record_info(fields, Tag),
        [_Tag | Values] = tuple_to_list(M),
        L = lists:zip(Fields, Values),
        maps:from_list(L)).

-spec moment_to_map(moment()) -> map().
moment_to_map(M) -> ?to_map(moment).
-spec user_to_map(user()) -> map().
user_to_map(M) -> ?to_map(user).
-spec device_to_map(device()) -> map().
device_to_map(M) -> ?to_map(device).

-spec map_to_moment(map()) -> moment() | {error, string()}.
map_to_moment(M = #{<<"name">> := Name,
                    <<"next_moment">> := Next,
                    <<"interval">> := Int,
                    <<"excl_days">> := ExclDays,
                    <<"excl_time">> := ExclTime,
                    <<"private">> := Priv}) when map_size(M) =:= 6 ->
    #moment{moment_id = unknown,
            name = Name,
            next_moment = Next,
            interval = binary_to_existing_atom(Int),
            excl_days = ExclDays,
            excl_time = ExclTime,
            private = Priv};
map_to_moment(_) ->
    {error, "Malformed Body"}.
