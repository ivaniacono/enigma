%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This is the supervisor of the Goals and of the Scheduler.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(goal_sup).

-behaviour(supervisor).

-include("Goal.hrl").

%%-export([child_list/1]).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    {ok, GoalsFunctionSet} = application:get_env(goals_function_set),
    start_link(GoalsFunctionSet).

start_link(GoalsFunctionSet) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, GoalsFunctionSet).

stop() ->
    exit(whereis(?MODULE), shutdown).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(GoalsFunctionSet) ->
%    RestartStrategy = one_for_one,
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
%    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 3600,
%%    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    io:format("EN.I.GM.A.~nErlaNg applIed to a Goal-Manager for Autonomous-systems~n"),

    goal_db:init_table(GoalsFunctionSet),

    ChildList = child_list(goal_db:key_list()) ++ [scheduler()],
    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% scheduler specifications
scheduler() ->
%%    Restart = permanent,
    Restart = transient,
    Shutdown = 2000,
    Type = worker,
    
    {scheduler, {scheduler, start_link, []}, Restart, Shutdown, Type, [scheduler]}.

%% goals specifications
child_list([]) ->
    [];
child_list([H|T]) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Goal = goal_db:lookup_id(H, nolist),
    Child = {Goal#goal.id, {goal, start_link, [Goal]},
	     Restart, Shutdown, Type, [goal]},
    [Child | child_list(T)].
