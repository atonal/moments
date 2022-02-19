-module(moments_data).

-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

%
% this way ---> strings map to either atom/string, therefore parsing needed
% it's also user input, so sanitize!
%
%              A   M1    B         M2          C
% json_text() --> map() --> moment_data_map() --> moment()
% (A) jsx:decode
% (B) moment_map_to_data_map - parse, sanitize, strings to atoms/strings etc.
% (C) moment_data_map_to_record
%
% M1: #{ bitstring() => Value }
% M2: #{ bitstring() => Value (correct type) }
%
% this way <--- atom/string maps to strings always, so just encode any map
%
%              D   M3    F
% json_text() <-- map() <-- moment()
% (D) jsx:encode
% (F) moment_record_to_map
%
% M3: #{ atom() => Value (correct type) }
%
% Should M2 = M3? where M3 = moment_data_map() with atom() keys
%

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
-spec parse_moment_map(map()) -> moment_data_map() | {error, bitstring()}.
parse_moment_map(M = #{<<"name">> := Name,
                    <<"next_moment">> := Next,
                    <<"interval">> := Int,
                    <<"excl_days">> := ExclDays,
                    <<"excl_time">> := ExclTime,
                    <<"private">> := Priv}) when
      map_size(M) =:= 6, % could we ignore other keys? we could, for forward compatibility
      is_bitstring(Name),
      is_integer(Next),
      is_bitstring(Int), % atom in moment() but in json it's a string
      is_list(ExclDays),
      is_list(ExclTime),
      is_atom(Priv) ->
    try
        #{moment_id => unknown,
          name => Name,
          next_moment => Next,
          interval => binary_to_existing_atom(Int),
          excl_days => ExclDays,
          excl_time => ExclTime,
          private => Priv}
    catch
        error:badarg:Stack ->
            ?LOG_DEBUG("moments_data: badarg - stack: ~p", [Stack]),
            case lists:keyfind(binary_to_existing_atom, 2, Stack) of
                {_,_,[AtomStr,_],_} when is_bitstring(AtomStr) ->
                    {error, <<"Invalid interval: ", AtomStr/binary>>};
                _ -> % false or {something}
                    {error, <<"Invalid interval: ", Int/binary>>}
            end
    end;
parse_moment_map(_) ->
    {error, <<"Invalid json">>}.

% Moment maps (from json) without ID
-spec map_to_moment(moment_data_map(), moment_id()) -> moment() | {error, atom()}.
map_to_moment(M = #{moment_id := unknown,
                    name := Name,
                    next_moment := Next,
                    interval := Int,
                    excl_days := ExclDays,
                    excl_time := ExclTime,
                    private := Priv},
              Mid) when
      map_size(M) =:= 7,
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
    {error, invalid_data_map}.
