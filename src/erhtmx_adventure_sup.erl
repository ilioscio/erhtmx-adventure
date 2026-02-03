%%%-------------------------------------------------------------------
%%% @doc erhtmx_adventure_sup
%%% Top-level supervisor for the ERHTMX Adventure application.
%%%
%%% This supervisor uses the 'one_for_one' strategy, meaning if a
%%% child process dies, only that process is restarted.
%%%
%%% Currently, the game is stateless on the server (state in cookies),
%%% so we don't need worker processes. This supervisor exists for
%%% future expansion (e.g., leaderboards, multiplayer).
%%% @end
%%%-------------------------------------------------------------------
-module(erhtmx_adventure_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% Called by the application module during startup.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Initializes the supervisor.
%% Returns the supervision strategy and child specifications.
%%
%% SupFlags:
%%   - strategy: one_for_one (restart only the failed child)
%%   - intensity: 5 (max 5 restarts...)
%%   - period: 10 (...within 10 seconds)
%%
%% ChildSpecs: Currently empty, but can add workers here later.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
