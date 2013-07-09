=============================================================================
_______ __   _   _____    ______ _______   _______
|______ | \  |     |     |  ____ |  |  |   |_____|
|______ |  \_| . __|__ . |_____| |  |  | . |     |

%% ErlaNg applIed to a Goal-Manager for Autonomous-systems
=============================================================================


ENIGMA is a Goal Manager written in Erlang/OTP and can be used in autonomous
systems like robots. It has the task of executing a set of goals ordered by
priority and feasibility, if one goal is not feasible or its execution fails,
it will be tried again later. Each goal has a priority that can depend on the
enviorment (for example a robot position), also one goal can be feasible or
not in a moment (for example because there is an obstacle).

WARNING: The developing is in progress, so using it for Production is not
recomended.

**HOW TO GET IT**

`git clone https://github.com/ivaniacono/enigma.git`

**HOW TO USE IT**

You need to insert only the code of the goals in a specific module called
goals function set, the default module is ./src/goals_function.erl (you can change
it, editing ./ebin/enigma.app). There each goal has to defined by writing:

- record function definition, containing the goal information,
- priority function, that needs to return an integer value (lower is better),
- feasibility function, that needs to return true or false,
- execute function, that needs to return success or fail.

ENIGMA supports two type of Goals: simple or combo (stands for combined).
Combo is a goal formed by other goals (subgoals) that can be simple or not, it has a
execution policy that can be:

- and_policy (the goals is executed when all subgoals are executed)
- xor_policy (the goal is executed when one subgoal is executed)

You can specify it in the record function definition, finally add the goal to
the init_goal() function.


**HOW TO COMPILE IT**

From the root directory of ENIGMA type:

$: make


**HOW TO RUN IT**

From the root directory of ENIGMA type:

$: ./enigma.sh

If you want redirect the output in a log file type:

$: ./enigma.sh > enigma.log
