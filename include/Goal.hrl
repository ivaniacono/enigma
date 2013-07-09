%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This is the record definition of the Goal.
%%% @end
%%%-------------------------------------------------------------------------------------

-record(goal, {id,                              %% atom
	      type,                             %% simple | {combo, xor_policy | and_policy, [SubGoalId]}
	      parent_id,                        %% atom, parent_id
	      subgoal = false,                  %% atom, true | false
	      state,                            %% everything
	      feasibility = undefined,          %% tuple, true | false
	      feasibility_function,             %% tuple, {Module, Function}
	      priority = -1,                    %% int
              priority_function,                %% tuple, {Module, Function}
	      execute,                          %% atom, success | fail
	      execute_function,                 %% tuple, {Module, Function}
	      trials = 0,                       %% int
	      max_trials = 0}).                 %% int
