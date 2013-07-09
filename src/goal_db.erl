%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module includes the functions of the DB.
%%% @end
%%%-------------------------------------------------------------------------------------

-module(goal_db).
-compile(export_all).

-include("Goal.hrl").

%% ETS table.
create_table() ->
    ets:new(goalTab, [named_table, set, public, {keypos, #goal.id}]).

close_table() ->
    ets:close(goalTab).

%% Initializes the table .
init_table(GoalsFunctionSet) ->
    create_table(),
    apply(GoalsFunctionSet, init_goal, []),
    subgoal_parent_calculate().

%% Adds a Goal to the ETS.
add_goal(Goal) ->
    ets:insert(goalTab, Goal).

%% Searches a Goal in the ETS.
lookup_id(Id, nolist) ->
    case ets:lookup(goalTab, Id) of
	[Goal] ->
	    Goal;
	[] ->
	    {error, noelement}
    end.

%% Return the keys of the ETS.
key_list() ->
    First = ets:first(goalTab),
    case First /= '$end_of_table' of
	true -> [First | key_list(First)];
	false -> []
    end.

key_list(Prev) ->
    Later = ets:next(goalTab, Prev),
    case Later /= '$end_of_table' of
	true -> [Later | key_list(Later)];
	false -> []
    end.

%% Calculates if one Goal is a subgoal and if true assigns its parent.
subgoal_parent_calculate() ->
    subgoal_parent_calculate(key_list()).

subgoal_parent_calculate([H | T]) ->
    G = lookup_id(H, nolist),
    case G#goal.type /= simple of
	true ->
	    {combo, _, SubGoalsList} = G#goal.type,
	    child_subgoal_parent_assignament(G#goal.id, SubGoalsList),
	    subgoal_parent_calculate(T);
	false ->
	    subgoal_parent_calculate(T)
    end;
subgoal_parent_calculate([]) ->
    ok.

child_subgoal_parent_assignament(Parent, [H | T]) ->
    G = lookup_id(H, nolist),
    add_goal(G#goal{subgoal = true, parent_id = Parent}),
    child_subgoal_parent_assignament(Parent, T);
child_subgoal_parent_assignament(_, []) ->
    ok.

%% Resets the priority of all Goals.
reset_priority() ->
    reset_priority(key_list()).

reset_priority([H | T]) ->
    G = lookup_id(H, nolist),
    case (G#goal.execute /= success) of
 	true ->
	    goal:reset_priority(G),
	    reset_priority(T);
	false ->
	    reset_priority(T)
    end;
reset_priority([]) ->
    true.

%% Calculates the priority of non executed Goals.
priority_calculate(Type) ->
    priority_calculate(Type, key_list()).

priority_calculate(all, List) ->
    priority_calculate(simple, List),
    priority_calculate(combo, List);
priority_calculate(simple, [H | T]) ->
    G = lookup_id(H, nolist),
    case (G#goal.type == simple) and (G#goal.execute /= success) of
	true ->
	    goal:priority(G),
	    priority_calculate(simple, T);
	false ->
	    priority_calculate(simple, T)
    end;
priority_calculate(simple, []) ->
    true;
priority_calculate(combo, [H | T]) ->
    G = lookup_id(H, nolist),
    case (G#goal.type /= simple) and (G#goal.execute /= success) of
	true ->
	    {P, _} = goal:priority(G),
	    case P > -1 of
		true ->
		    priority_calculate(combo, T);
		false ->
		    priority_calculate(combo, T ++ [H])
	    end;
	false ->
	    priority_calculate(combo, T)
    end;
priority_calculate(combo, []) ->
    true.

%% Orders a list of Goals by priority.
priority_ordered_list(List) ->
    L = lists:keysort(2, priority_ordered_tuple(List)),
    ExtractKey = fun(X) ->
			 {Key, _} = X,
			 Key
		 end,
    lists:map(ExtractKey, L).

priority_ordered_tuple([H | T]) ->
    G = lookup_id(H, nolist),
    Tuple = {G#goal.id, G#goal.priority},
    [Tuple | priority_ordered_tuple(T)];
priority_ordered_tuple([]) ->
    [].

%% Calculates the maximum number of trials to do.
max_trials() ->
    MaxTrials = fun(X) ->
			G = lookup_id(X, nolist),
			G#goal.max_trials
		end,
    lists:max(lists:map(MaxTrials, key_list())).
