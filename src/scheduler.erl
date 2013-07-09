%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module includes the istructions to schedule and execute a set of Goals.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(scheduler).

-include("Goal.hrl").

-export([start_link/0, init/1]).

%% @doc Starts the scheduler.
%% @end
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    timer:sleep(100), %% waits for ets initialization
    io:format("== Start scheduling ========================~n"),
    schedule().


%% @doc Executes the set of Goals.
%% @end
schedule() ->
    goal_db:reset_priority(),
    goal_db:priority_calculate(all),
    Goal = next_goal(),
    case Goal == goals_completed of
	true -> goals_completed,
		io:format("== Scheduling completed ====================~nbye!~n"),
		application:stop(enigma);		
%%		start_enigma:stop();
	false ->
%	   timer:sleep(5000), %% this timer is for debugging
%	   io:format("** Feasibility ~p...",[Goal#goal.id]),
	   case goal:feasibility(Goal) of
	       {true, _} ->
%		   io:format("true~n"),
		   io:format("** Execute ~p...",[Goal#goal.id]),
		   case goal:execute(Goal) of
		       {success, _} ->
			   io:format("success!~n");
		       {fail, _} ->
			   io:format("fail!~n"),
			   goal:increment_trials(Goal)
		   end;
	       {false, _} ->
%		   io:format("false~n"),
		   goal:increment_trials(Goal)
	    end,
	    schedule()
    end.

%% @doc Choooses the next goal to execute.
%% @end
next_goal() ->
    KeyList = goal_db:priority_ordered_list(goal_db:key_list()),
    next_goal(KeyList, KeyList, 0, goal_db:max_trials()).

next_goal(KeyList, [H | T], Trial, MaxTrials) ->
    Goal = goal_db:lookup_id(H, nolist),
    case (Trial < Goal#goal.max_trials) and (Goal#goal.trials == Trial) and
        (Goal#goal.execute /= success) and (Goal#goal.subgoal == false) of
	true ->
	    Goal;
	false ->
	    next_goal(KeyList, T, Trial, MaxTrials)
    end;
next_goal(KeyList, [], Trial, MaxTrials) when Trial < MaxTrials ->
    next_goal(KeyList, KeyList, Trial+1, MaxTrials);
next_goal(_KeyList, [], _Trial, _MaxTrials) ->
    goals_completed.
