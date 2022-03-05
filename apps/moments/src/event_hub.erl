-module(event_hub).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/0, subscribe/2, unsubscribe/1, notify_moment/1, log_handler/1]).

-type handler() :: {atom(), reference()}.

start_link() ->
    gen_event:start_link({local, moment_man}).

-spec subscribe(pid(), moment_id()) -> handler().
subscribe(Pid, MomentId) ->
    ?LOG_INFO("event_hub: Pid ~p subscribed for moment ~p!", [Pid, MomentId]),
    HandlerId = {event_sub, make_ref()},
    gen_event:add_handler(moment_man, HandlerId, {Pid, MomentId}),
    HandlerId.

-spec unsubscribe(handler()) -> term().
unsubscribe(HandlerId) ->
    ?LOG_INFO("event_hub: Unsibscribing ~p!", [HandlerId]),
    gen_event:delete_handler(moment_man, HandlerId, unsubscribe).

-spec notify_moment(moment()) -> ok.
notify_moment(Moment) ->
    gen_event:notify(moment_man, {moment, Moment}).

-spec log_handler(on|off) -> term().
log_handler(on) ->
    case lists:member(logger_handler, gen_event:which_handlers(moment_man)) of
        true ->
            ?LOG_NOTICE("event_hub: logger_handler already exists!"),
            ok;
        false ->
            gen_event:add_handler(moment_man, logger_handler, [])
    end;
log_handler(off) ->
    gen_event:delete_handler(moment_man, logger_handler, []).
