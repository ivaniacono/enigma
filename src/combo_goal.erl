%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module includes the instructions to execute combined Goals.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(combo_goal).

-compile(export_all).

-include("Goal.hrl").

%% @doc Feasibility function of the combined Goal.
%% Goal is feasible if there is one subgoal feasible.
%% @end
combo_feasibility(List, State) ->
    Feasibility = fun(X) ->
			 G = goal_db:lookup_id(X, nolist),
			 case goal:feasibility(G) of
			     {true, _} ->
				 true;
			     {false, _} ->
				 false
			 end
		  end,
    {lists:member(true, lists:map(Feasibility, List)), State}.

%% @doc Priority function of the combined Goal.
%% Goal priority is the lowest priority of the subgoals.
%% @end
combo_priority(List, State) ->
    Priority = fun(X) ->
		       G = goal_db:lookup_id(X, nolist),
		       G#goal.priority
	       end,
    {lists:min(lists:map(Priority, List)), State}.


%% @doc Execute function of the combined Goal.
%% When the policy is and_policy the Goal is executed when all subgoals are executed.
%% When the policy is xor_policy the Goal is executed when one subgoal is executed.
%% @end
combo_execute(and_policy, List, State) ->
    Execute = fun(X) ->
		      G = goal_db:lookup_id(X, nolist),
		      case G#goal.feasibility of
			  true ->
			      {Result, _} = goal:execute(G),
			      Result;
			  false ->
			      fail
		      end
	      end,
    case lists:member(fail, lists:map(Execute, goal_db:priority_ordered_list(List))) of
	true ->
	    {fail, State};
	false ->
	    {success, State}
    end;
combo_execute(xor_policy, List, State) ->
    combo_execute_xor(goal_db:priority_ordered_list(List), State).

combo_execute_xor([H | T], State) ->
    G = goal_db:lookup_id(H, nolist),
    case G#goal.feasibility of
	true ->
	    {Result, _} = goal:execute(G),
	    case Result of
		success ->
		    {success, State};
		fail ->
		    combo_execute_xor(T, State)
	    end;
	false ->
	    combo_execute_xor(T, State)
    end;
combo_execute_xor([], State) ->
    {fail, State}.
