%%%-------------------------------------------------------------------
%% @doc moments top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(moments_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [#{id => event_hub,
                    start => {event_hub, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => dynamic},
                  #{id => moment_dispatcher,
                    start => {moment_dispatcher, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [moment_dispatcher]},
                  #{id => watcher,
                    start => {watcher, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [watcher]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
