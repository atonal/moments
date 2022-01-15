-module(logger_handler).
-behaviour(gen_event).
-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_event/2, handle_call/2, terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event({moment, Moment}, State) ->
    ?LOG_NOTICE("logger_handler: moment: ~p", [Moment]),
    {ok, State};
handle_event(Event, State) ->
    ?LOG_NOTICE("logger_handler: unknown event ~p", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    ?LOG_NOTICE("logger_handler: call ~p", [Request]),
    {ok, ok, State}.

terminate(_Args, _State) ->
    ok.
