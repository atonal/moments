-module(event_hub).
-include("data_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([subscribe/2, unsubscribe/1, notify_moment/1]).

-type handler() :: {atom(), reference()}.

-spec subscribe(pid(), moment()) -> handler().
subscribe(Pid, MomentId) ->
    HandlerId = {event_sub, make_ref()},
    gen_event:add_handler(moment_man, HandlerId, {Pid, MomentId}),
    HandlerId.

-spec unsubscribe(handler()) -> term().
unsubscribe(HandlerId) ->
    gen_event:delete_handler(moment_man, HandlerId, unsubscribe).

-spec notify_moment(moment()) -> ok.
notify_moment(Moment) ->
    gen_event:notify(moment_man, {moment, Moment}).
