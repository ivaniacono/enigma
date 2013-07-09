{application, enigma,
[{description, "ErlaNg applIed to a Goal-Manager for Autonomous-systems"},
{vsn, "1.0"},
{modules, [goal, goal_sup, scheduler, goal_db, combo_goal, goal_function, enigma_app]},
{registered, [scheduler, goal_sup, enigma]},
{applications, [kernel, stdlib]},
{env, [{goals_function_set, goal_function}]},
{mod, {enigma_app,[]}}
]}.
