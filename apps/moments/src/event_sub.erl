-module(event_sub).
-behaviour(gen_event).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_event/2, handle_call/2, terminate/2]).

-type state() :: {pid(), moment()}.

-spec init(state()) -> {ok, state()}.
init(State) ->
    {ok, State}.

-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Event = {moment, Moment=#moment{moment_id=MomentId}},
             State = {Pid, MomentId}) ->
    ?LOG_NOTICE("event_sub: moment: ~p", [Moment]),
    Pid ! Event,
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(Request, State) ->
    ?LOG_NOTICE("event_sub: call ~p", [Request]),
    {ok, ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Args, _State) ->
    ok.
