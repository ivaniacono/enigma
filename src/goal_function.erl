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

%%--------------------------------------------------------------------------
%% @doc
%% Allow to the Goal to be scheduled adding it to the init_goal() function.
%%
%% @spec goal_db:add_goal(goalname()).
%% @end
%%--------------------------------------------------------------------------

%% -- Example --
%% init_goal() ->
%%    goal_db:add_goal(goalname()).

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

%% -- Example --
%% goalname() ->
%%     #goal{id = goalname,
%%	   type = simple,
%%	   state = state,
%%	   feasibility_function = {?MODULE, goalname_feasibility},
%%	   priority_function = {?MODULE, goalname_priority},
%%	   execute_function = {?MODULE, goalname_execute},
%%	   max_trials = 7}.

%%-----------------------------------------------------------------------
%% @doc
%5 Define a Goal Feasibility function.
%%
%% goalname_feasibility(_Goal, State) -> {true, State} | {false, State}
%% @end
%%-----------------------------------------------------------------------

%% -- Example --
%% goalname_feasibility(_Goal, State) ->
%%     case there_is_an_obstacle() of
%%	   true ->
%%	       {false, State};
%%	   false ->
%%	       {true, State}
%% end.

%%-----------------------------------------------------------------
%% @doc
%% Define a Goal Priority function.
%%
%% @spec goalname_priority(_Goal, State) -> {int, State}
%% @end
%%-----------------------------------------------------------------

%% -- Example --
%% goalname_priority(_Goal, State) ->
%%     case distance_from_something() < 100 of
%%	 true ->
%%	     {1, State};
%%	 false ->
%%	     {5, State}
%%     end.

%%--------------------------------------------------------------------------
%% @doc
%% Define a Goal Execute function.
%%
%% @spec goalname_execute(_Goal, State) -> {success, State} | {fail, State}
%% @end
%%--------------------------------------------------------------------------

%% -- Example --
%% goalname_execute(_Goal, State) ->
%%     motor:go_to_point(30, 50),
%%     motor:left_servo(90),
%%     motor:rotate_left(20),
%%     motor:left_servo(0),
%%     {success, State}.
