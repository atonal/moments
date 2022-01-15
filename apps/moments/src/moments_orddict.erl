-module(moments_orddict).

% orddict with custom order function
% ordered by next_moment, indexed by moment_id

-export([new/0, from_list/1, store/3, store_list/2]).
-export_type([orddict/0, orddict/2]).

-type orddict() :: orddict(_, _).
-type orddict(Key, Value) :: [{Key, Value}].

-spec new() -> orddict().
new() -> [].

-spec from_list(List) -> Orddict when
      List :: [{Key, Value}],
      Orddict :: orddict(Key, Value).
from_list(List) ->
    lists:sort(fun({_, A}, {_, B}) -> data_utils:is_before(A, B) end, List).

-spec store(Key, Value, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).
store(Key, Value, OrdDict) ->
    Dict = dict:from_list(OrdDict),
    NewDict = dict:store(Key, Value, Dict),
    NewOrdDict = dict:to_list(NewDict),
    lists:sort(fun({_, A}, {_, B}) -> data_utils:is_before(A, B) end, NewOrdDict).

-spec store_list([{Key, Value}], Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).
store_list([{Key, Value}|T], OrdDict) ->
    store_list(T, store(Key, Value, OrdDict));
store_list([], OrdDict) ->
    OrdDict.
