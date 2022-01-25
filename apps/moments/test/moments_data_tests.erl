-module(moments_data_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

moment_to_map_test_() ->
    {"moment_to_map tests", moment_to_map()}.
moment_to_map() ->
    [{"moment to map", ?_assertEqual(#{moment_id => 2,
                                       name => "moment2",
                                       next_moment => 60,
                                       interval => daily,
                                       excl_days => [],
                                       excl_time => [],
                                       private => true},
                                     moments_data:moment_to_map(
                                       #moment{moment_id = 2,
                                               name = "moment2",
                                               next_moment = 60,
                                               interval = daily,
                                               excl_days = [],
                                               excl_time = [],
                                               private = true}))}].

user_to_map_test_() ->
    {"user_to_map tests", user_to_map()}.
user_to_map() ->
    [{"user to map", ?_assertEqual(#{user_id => 2,
                                     name => "user 2"},
                                   moments_data:user_to_map(
                                     #user{user_id = 2,
                                           name = "user 2"}))}].

device_to_map_test_() ->
    {"device_to_map tests", device_to_map()}.
device_to_map() ->
    [{"device to map", ?_assertEqual(#{device_id => 3,
                                       name => "device 1"},
                                     moments_data:device_to_map(
                                       #device{device_id = 3,
                                               name = "device 1"}))}].
