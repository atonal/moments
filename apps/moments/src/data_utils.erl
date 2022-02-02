-module(data_utils).
-export([is_passed/2, is_before/2, time_add/2, get_next_moment/2]).

-include("data_records.hrl").

-spec is_passed(moment(), utc_time()) -> boolean().
is_passed(Moment, Time) ->
    Moment#moment.next_moment =< Time.

-spec is_before(moment(), moment()) -> boolean().
is_before(Moment1, Moment2) ->
    is_passed(Moment1, Moment2#moment.next_moment).

-define(sec_to_min(S), (60 * S)).
-define(sec_to_hour(S), (60 * ?sec_to_min(S))).
-define(sec_to_day(S), (24 * ?sec_to_hour(S))).
-define(sec_to_week(S), (7 * ?sec_to_day(S))).
-define(sec_to_month(S), (30 * ?sec_to_day(S))).
-define(sec_to_year(S), (365 * ?sec_to_day(S))).

-type second() :: {second, non_neg_integer()}.
-type minute() :: {minute, non_neg_integer()}.
-type hour() :: {hour, non_neg_integer()}.
-type day() :: {day, non_neg_integer()}.
-type week() :: {week, non_neg_integer()}.
-type month() :: {month, non_neg_integer()}.
-type year() :: {year, non_neg_integer()}.
-type timeinterval() :: second() | minute() | hour() | day() | week() | month() | year().

-spec time_add(utc_time(), timeinterval()) -> utc_time().
time_add(Time, {Unit, Value}) ->
    case Unit of
        second ->
            Time + Value;
        minute ->
            Time + ?sec_to_min(Value);
        hour ->
            Time + ?sec_to_hour(Value);
        day ->
            Time + ?sec_to_day(Value);
        week ->
            Time + ?sec_to_week(Value);
        month ->
            Time + ?sec_to_month(Value);
        year ->
            Time + ?sec_to_year(Value)
    end.

-spec get_next_moment(moment(), integer()) -> next_moment().
get_next_moment(#moment{next_moment=Next, interval=Interval}, CurTime) ->
    Base = lists:max([Next, CurTime]),
    case Interval of
        debug ->
            time_add(Base, {minute, 1});
        hourly ->
            time_add(Base, {hour, 1});
        daily ->
            time_add(Base, {day, 1});
        weekly ->
            time_add(Base, {week, 1});
        monthly ->
            time_add(Base, {month, 1});
        yearly ->
            time_add(Base, {year, 1})
    end.
