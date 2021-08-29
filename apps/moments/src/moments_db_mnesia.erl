-module(moments_db_mnesia).

-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([insert_user/2, remove_user/1]).

insert_user(Uid, Name) ->
    F = fun() ->
                  case mnesia:read({user, Uid}) =:= [] of
                      true ->
                          User = #user{user_id=Uid, name=Name},
                          mnesia:write(User);
                      false ->
                          ?LOG_ERROR("User ~ts already exists", [Uid]),
                          {error, user_exists}
                  end
          end,
    mnesia:transaction(F).

remove_user(Uid) ->
    F = fun() ->
                  case mnesia:read({user, Uid}) =/= [] of
                      true ->
                          mnesia:delete({user, Uid});
                      false ->
                          ?LOG_ERROR("No user ~ts to delete", [Uid]),
                          {error, user_doesnt_exists}
                  end
        end,
    mnesia:transaction(F).
