-module(moments_orddict_tests).
-include("../src/data_records.hrl").
-include_lib("eunit/include/eunit.hrl").

from_list_test_() ->
    {"from_list tests", from_list()}.
from_list() ->
    [{"unordered list", ?_assertEqual([{2, #moment{ next_moment=1}},
                                       {3, #moment{ next_moment=2}},
                                       {1, #moment{ next_moment=3}} ],
                                      moments_orddict:from_list([{1, #moment{ next_moment=3}},
                                                                 {2, #moment{ next_moment=1}},
                                                                 {3, #moment{ next_moment=2}}]))}].

store_test_() ->
    {"store tests", store()}.
store() ->
    [{"store in beginning",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {2, #moment{ next_moment=2}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:store(1, #moment{ next_moment=1},
                                          [{2, #moment{ next_moment=2}},
                                           {3, #moment{ next_moment=3}}]))},
     {"store in middle",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {2, #moment{ next_moment=2}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:store(2, #moment{ next_moment=2},
                                          [{1, #moment{ next_moment=1}},
                                           {3, #moment{ next_moment=3}}]))},
     {"store in end",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {2, #moment{ next_moment=2}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:store(3, #moment{ next_moment=3},
                                          [{1, #moment{ next_moment=1}},
                                           {2, #moment{ next_moment=2}}]))},
     {"replace with existing key",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {3, #moment{ next_moment=3}},
                     {2, #moment{ next_moment=200}}],
                    moments_orddict:store(2, #moment{ next_moment=200},
                                          [{1, #moment{ next_moment=1}},
                                           {2, #moment{ next_moment=2}},
                                           {3, #moment{ next_moment=3}}]))}].

store_list_test_() ->
    {"store list tests", store_list()}.
store_list() ->
    [{"store a list",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {2, #moment{ next_moment=2}},
                     {3, #moment{ next_moment=3}},
                     {4, #moment{ next_moment=4}}],
                    moments_orddict:store_list([{1, #moment{ next_moment=1}},
                                                {3, #moment{ next_moment=3}}],
                                               [{2, #moment{ next_moment=2}},
                                                {4, #moment{ next_moment=4}}]))}].

erase_test_() ->
    {"erase tests", orddict_erase()}.
orddict_erase() ->
    [{"erase in beginning",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:erase(2, [{1, #moment{ next_moment=1}},
                                              {2, #moment{ next_moment=2}},
                                              {3, #moment{ next_moment=3}}]))},
     {"erase nonexisting key is no-op",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:erase(2, [{1, #moment{ next_moment=1}},
                                              {3, #moment{ next_moment=3}}]))}].

erase_list_test_() ->
    {"erase list tests", erase_list()}.
erase_list() ->
    [{"erase a list",
      ?_assertEqual([{1, #moment{ next_moment=1}},
                     {3, #moment{ next_moment=3}}],
                    moments_orddict:erase_list([2,4], [{1, #moment{ next_moment=1}},
                                                       {2, #moment{ next_moment=2}},
                                                       {3, #moment{ next_moment=3}},
                                                       {4, #moment{ next_moment=4}}]))}].
