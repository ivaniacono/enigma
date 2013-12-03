%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This is the gen_server of the Goal. Eeach Goal is a gen_server.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(goal).

-behaviour(gen_server).

-include("Goal.hrl").

%% API
-export([start_link/1, stop/1]).
-export([feasibility/1, execute/1, priority/1, reset_priority/1, increment_trials/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

%% @doc Starts the Goal as gen_server.
%% @end
start_link(Goal) ->
    gen_server:start_link({local, Goal#goal.id}, ?MODULE, Goal, []).

stop(Goal) -> gen_server:cast(Goal#goal.id, stop).

%% @doc Executes the feasibility function of the Goal.
%% @end
feasibility(Goal) ->
    gen_server:call(Goal#goal.id, {feasibility, Goal#goal.type}, infinity).

%% @doc Resets the priority of the Goal.
%% @end
reset_priority(Goal) ->
    gen_server:call(Goal#goal.id, reset_priority).

%% @doc Executes the priority function of the Goal.
%% @end
priority(Goal) ->
    gen_server:call(Goal#goal.id, {priority, Goal#goal.type}, infinity).

%% @doc Executes the Goal.
%% @end
execute(Goal) ->
    gen_server:call(Goal#goal.id, {execute, Goal#goal.type}, infinity).

%% @doc Increments the counter of trials of the Goal.
%% @end
increment_trials(Goal) ->
    gen_server:call(Goal#goal.id, increment_trials).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Goal) ->
    {ok, Goal}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% HANDLE FEASIBILITY
handle_call({feasibility, simple}, _From, State) ->
    {Module, Function} = State#goal.feasibility_function,
    Arguments = [State, State#goal.state],
    {Result, GoalState} = apply(Module, Function, Arguments),
    NewState = State#goal{feasibility=Result, state=GoalState},
    goal_db:add_goal(NewState),
    Reply = {Result, GoalState},
    {reply, Reply, NewState};

handle_call({feasibility, {combo, _Policy, GoalsId}}, _From, State) ->
    Arguments = State#goal.state,
    {Result, GoalState} = combo_goal:combo_feasibility(GoalsId, Arguments),
    NewState = State#goal{feasibility=Result, state=GoalState},
    goal_db:add_goal(NewState),
    Reply = {Result, GoalState},
    {reply, Reply, NewState};

%% HANDLE RESET PRIORITY
handle_call(reset_priority, _From, State) ->
    NewState = State#goal{priority=-1},
    goal_db:add_goal(NewState),
    Reply = true,
    {reply, Reply, NewState};

%% HANDLE INCREMENT TRIAL
handle_call(increment_trials, _From, State) ->
    NewState = State#goal{trials=State#goal.trials + 1},
    goal_db:add_goal(NewState),
    Reply = true,
    {reply, Reply, NewState};

%% HANDLE PRIORITY
handle_call({priority, simple}, _From, State) ->
    {Module, Function} = State#goal.priority_function,
    Arguments = [State, State#goal.state],
    {P, GoalState} = apply(Module, Function, Arguments),
    NewState = State#goal{priority=P, state=GoalState},
    goal_db:add_goal(NewState),
    Reply = {P, GoalState},
    {reply, Reply, NewState};

handle_call({priority, {combo, _Policy, GoalsId}}, _From, State) ->
    Arguments = State#goal.state,
    {P, GoalState} = combo_goal:combo_priority(GoalsId, Arguments),
    NewState = State#goal{priority=P, state=GoalState},
    goal_db:add_goal(NewState),
    Reply = {P, GoalState},
    {reply, Reply, NewState};

%% HANDLE EXECUTE
handle_call({execute, simple}, _From, State) ->
    {Module, Function} = State#goal.execute_function,
    Arguments = [State, State#goal.state],
    {Result, GoalState} = apply(Module, Function, Arguments),
    NewState = State#goal{execute=Result, state=GoalState},
    goal_db:add_goal(NewState),
    Reply={Result, GoalState},
    {reply, Reply, NewState};

handle_call({execute, {combo, Policy, GoalsId}}, _From, State) ->
    Arguments = State#goal.state,
    {Result, GoalState} = combo_goal:combo_execute(Policy, GoalsId, Arguments),
    NewState = State#goal{execute=Result, state=GoalState},
    goal_db:add_goal(NewState),
    Reply={Result, GoalState},
    {reply, Reply, NewState}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
