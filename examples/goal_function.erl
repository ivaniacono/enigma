%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module is the goal function set, includes the istructions of the Goals.
%%% Define your goals and insert its code here.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(goal_function).
-compile(export_all).

-include("Goal.hrl").

%%--------------------------------------------
%% @doc
%% Allow to the Goal to be scheduled.
%%
%% @spec goal_db:add_goal(goalname()).
%% @end
%%--------------------------------------------

init_goal() ->
    goal_db:add_goal(totem()),
    goal_db:add_goal(map()),
    goal_db:add_goal(bottle()),
    goal_db:add_goal(combo_goal()),
    goal_db:add_goal(glass()),
    goal_db:add_goal(present()),
    goal_db:add_goal(combo_goal_two()).

%%---------------------------------------------------------------------------------
%% @doc
%% Define a Goal.
%%
%% @spec
%% Define a Simple Goal
%% goalname() ->
%%    #goal{id = goalname,
%%          type = simple,
%%          state = State,
%%          feasibility_function = {Module, Function},
%%          priority_function = {Module, Function},
%%          execute_function = {Module, Function},
%%          max_trials = int }.
%%
%% Define a Combined Goal
%% goalname() ->
%%    #goal{id = goalname,
%%          type = {combo, and_policy | xor_policy, [SubGoalId]},
%%          max_trials = int }.
%% @end 
%%---------------------------------------------------------------------------------

bottle() ->
    #goal{id = bottle,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, bottle_feasibility},
	  priority_function = {?MODULE, bottle_priority},
	  execute_function = {?MODULE, bottle_execute},
	  max_trials = 7}.

totem() ->
    #goal{id = totem,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, totem_feasibility},
	  priority_function = {?MODULE, totem_priority},
	  execute_function = {?MODULE, totem_execute},
	  max_trials = 10}.

map() ->
    #goal{id = map,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, map_feasibility},
	  priority_function = {?MODULE, map_priority},
	  execute_function = {?MODULE, map_execute},
	  max_trials = 10}.

glass() ->
    #goal{id = glass,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, glass_feasibility},
	  priority_function = {?MODULE, glass_priority},
	  execute_function = {?MODULE, glass_execute},
	  max_trials = 10}.

present() ->
    #goal{id = present,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, present_feasibility},
	  priority_function = {?MODULE, present_priority},
	  execute_function = {?MODULE, present_execute},
	  max_trials = 10}.

combo_goal() ->
    #goal{id = combo_goal,
	  type = {combo, xor_policy, [totem, map]},
	  max_trials = 10}.

combo_goal_two() ->
    #goal{id = combo_goal_two,
	  type = {combo, xor_policy, [present, glass]},
	  max_trials = 5}.

%%-----------------------------------------------------------------------
%% @doc
%5 Define a Goal Feasibility function.
%%
%% goalname_feasibility(_Goal, State) -> {true, State} | {false, State}
%% @end
%%-----------------------------------------------------------------------

bottle_feasibility(_Goal, State) ->
    {true, State}.

totem_feasibility(_Goal, State) ->
    {true, State}.
%%    {false, State}.

map_feasibility(_Goal, State) ->
    {true, State}.
%%    {false, State}.
glass_feasibility(_Goal, State) ->
    {true, State}.
%%    {false, State}.
present_feasibility(_Goal, State) ->
    {true, State}.

%%-----------------------------------------------------------------
%% @doc
%% Define a Goal Priority function.
%%
%% @spec goalname_priority(_Goal, State) -> {int, State}
%% @end
%%-----------------------------------------------------------------

bottle_priority(_Goal, State) ->
    {4, State}.

totem_priority(_Goal, State) ->
    {6, State}.

map_priority(_Goal, State) ->
    {7, State}.

glass_priority(_Goal, State) ->
    {1, State}.

present_priority(_Goal, State) ->
    {3, State}.

%%--------------------------------------------------------------------------
%% @doc
%% Define a Goal Execute function.
%%
%% @spec goalname_execute(_Goal, State) -> {success, State} | {fail, State}
%% @end
%%--------------------------------------------------------------------------

bottle_execute(_Goal, State) ->
%    {success, State}.
    {fail, State}.

totem_execute(_Goal, State) ->
%    {success, State}.
    {fail, State}.

map_execute(_Goal, State) ->
    {success, State}.
%    {fail, State}.

glass_execute(_Goal, State) ->
    {success, State}.

present_execute(_Goal, State) ->
%    {success, State}.
    {fail, State}.
