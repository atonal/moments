-module(moments_data).

-include_lib("kernel/include/logger.hrl").
-include("data_records.hrl").

%
% this way ---> strings map to either atom/string, therefore parsing needed
% it's also user input, so sanitize!
%
%              A   M1    B
% json_text() --> map() --> moment()
% (A) jsx:decode
% (B) moment_map_to_data_map - parse, sanitize, strings to atoms/strings etc.
%
% M1: generic map #{ bitstring() => Value }
%
% this way <--- atom/string maps to strings always, so just encode any map
%
%              D   M2    F
% json_text() <-- map() <-- moment()
% (D) jsx:encode
% (F) moment_record_to_map
%
% M2: generic map #{ atom() => Value }

-export([moment_to_map/1]).
-export([user_to_map/1]).
-export([device_to_map/1]).
-export([parse_moment_map/1]).
-export([parse_user_map/1]).
-export([moments_with_links_to_jsonapi_map/1]).

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

% generic map into moment()
-spec parse_moment_map(map()) -> moment() | {error, bitstring()}.
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
        #moment{moment_id = unknown,
          name = Name,
          next_moment = Next,
          interval = erlang:binary_to_existing_atom(Int),
          excl_days = ExclDays,
          excl_time = ExclTime,
          private = Priv}
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

% generic map into user()
-spec parse_user_map(map()) -> user() | {error, bitstring()}.
parse_user_map(#{<<"name">> := Name}) when
      is_bitstring(Name) ->
    #user{user_id = unknown,
          name = Name};
parse_user_map(_) ->
    {error, <<"Invalid json">>}.

-spec moments_with_links_to_jsonapi_map(moment_with_links()) -> map().
moments_with_links_to_jsonapi_map(MomentsWLinks) ->
    #{
      <<"data">> =>
      [
       #{
         <<"type">> => moments,
         <<"id">> => M#moment.moment_id,
         <<"attributes">> => maps:without([moment_id], moment_to_map(M)),
         <<"relationships">> =>
         #{
           <<"follows">> =>
           #{
             <<"data">> =>
             [#{<<"type">> => user, <<"id">> => F#follows.user} ||
              F <- L, is_record(F, follows)]
            },
           <<"admin">> =>
           #{
             <<"data">> =>
             [#{<<"type">> => user, <<"id">> => A#admin_of.user} ||
              A <- L, is_record(A, admin_of)]
            }
          }
        } || {M, L} <- maps:to_list(MomentsWLinks)
      ]
     }.
