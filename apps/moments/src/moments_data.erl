-module(moments_data).

-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

-export([moment_to_map/1]).
-export([user_to_map/1]).
-export([device_to_map/1]).
-export([parse_moment_map/1]).
-export([map_to_moment/2]).

-define(to_map(Tag),
        Fields = record_info(fields, Tag),
        [_Tag | Values] = tuple_to_list(M),
        L = lists:zip(Fields, Values),
        maps:from_list(L)).

% Full moments with ID
-spec moment_to_map(moment()) -> map().
moment_to_map(M) -> ?to_map(moment).
-spec user_to_map(user()) -> map().
user_to_map(M) -> ?to_map(user).
-spec device_to_map(device()) -> map().
device_to_map(M) -> ?to_map(device).

% generic map into moment_data_map()
-spec parse_moment_map(map()) -> moment_data_map() | {error, atom(), any()}.
parse_moment_map(M = #{<<"name">> := Name,
                    <<"next_moment">> := Next,
                    <<"interval">> := Int,
                    <<"excl_days">> := ExclDays,
                    <<"excl_time">> := ExclTime,
                    <<"private">> := Priv}) when
      map_size(M) =:= 6, % could we ignore other keys?
      is_bitstring(Name),
      is_integer(Next),
      is_bitstring(Int), % atom in moment() but in json it's a string
      is_list(ExclDays),
      is_list(ExclTime),
      is_atom(Priv) ->
    try
        M#{<<"interval">> => binary_to_existing_atom(Int)}
    catch
        error:badarg:Stack ->
            ?LOG_DEBUG("moments_data: badarg - stack: ~p", [Stack]),
            case lists:keyfind(binary_to_existing_atom, 2, Stack) of
                {_,_,[AtomStr,_],_} ->
                    {error, malformed_body, << <<"Invalid value: ">>/binary, AtomStr/binary>>};
                _ -> % false or {something}
                    {error, malformed_body, "Badarg"}
            end
    end;
parse_moment_map(_) ->
    {error, malformed_body, "Invalid json"}.

% Moment maps (from json) without ID
-spec map_to_moment(moment_data_map(), moment_id()) -> moment() | {error, atom(), any()}.
map_to_moment(M = #{<<"name">> := Name,
                    <<"next_moment">> := Next,
                    <<"interval">> := Int,
                    <<"excl_days">> := ExclDays,
                    <<"excl_time">> := ExclTime,
                    <<"private">> := Priv},
              Mid) when
      map_size(M) =:= 6,
      is_bitstring(Name),
      is_integer(Next),
      is_atom(Int),
      is_list(ExclDays),
      is_list(ExclTime),
      is_atom(Priv) ->
        #moment{moment_id = Mid,
                name = Name,
                next_moment = Next,
                interval = Int,
                excl_days = ExclDays,
                excl_time = ExclTime,
                private = Priv};
map_to_moment(_, _) ->
    {error, malformed_body, "Invalid json"}.
