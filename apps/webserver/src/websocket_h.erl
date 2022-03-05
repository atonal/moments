-module(websocket_h).

-include_lib("kernel/include/logger.hrl").
-include("../../moments/src/data_records.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_info/2]).
-export([websocket_handle/2]).
-export([terminate/3]).

-type state() :: #{
                   user => user_id(),
                   handlers => list()
                  }.

-spec init(any(), state()) -> {term(), term(), state(), term()}.
init(Req, State) ->
    Opts = #{compress => true,
            idle_timeout => 60000,
            max_frame_size => 20000 % TODO: define
            },
    UserId = cowboy_req:binding(id, Req),
    ?LOG_INFO("websocket for user ~p!", [UserId]),
    {cowboy_websocket, Req, State#{user => UserId}, Opts}.

-spec websocket_init(state()) -> {term(), state()}.
websocket_init(#{user := UserId} = State) ->
    ?LOG_INFO("websocket init for user ~p!", [UserId]),
    Moments = moments_db_mnesia:get_followed_moments(UserId),
    HandlerIds = lists:foldl(
                   fun(M, AccIn) ->
                           AccIn ++ [event_hub:subscribe(self(), M#moment.moment_id)]
                   end,
                   [],
                   Moments),
    {ok, State#{handlers => HandlerIds}}.

% Ignore received stuff
websocket_handle(_Frame, State) ->
        {ok, State}.

-spec websocket_info(term(), state()) -> {term(), state()}.
websocket_info({moment, Moment}, State) ->
    ?LOG_INFO("websocket moment: ~p!", [Moment]),
    {[{text, ["moment ", integer_to_binary(Moment#moment.moment_id)]}], State};
websocket_info(Data, State) ->
    ?LOG_INFO("websocket msg: ~p!", [Data]),
    {ok, State}.

terminate(Reason, _PartialReq, #{handlers := HList} = State) when length(HList) >= 0 ->
    ?LOG_INFO("websocket terminate, reason: ~p!", [Reason]),
    ?LOG_DEBUG("websocket release state: ~p!", [State]),
    lists:map(fun(H) -> event_hub:unsubscribe(H) end,
              HList),
    ok;
terminate(Reason, _PartialReq, _State) ->
    ?LOG_INFO("websocket terminate, reason: ~p!", [Reason]),
    ok.
