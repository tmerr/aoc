# Day 20

Today's problem is about finding the number of cheats in a maze that save a certain amount of cost.  I boil it down to

Given a start position `S`, end position `E`, cheat duration `D`, integer `C` (required savings), find the number of vertex pairs `(U, V)` subject to cheat duration constraint `manhattan_distance(U, V) <= D` and cost constraint `cheat_cost(U, V) <= shortest_path_cost(S, E) - C`. 

A path through the maze with cheats enabled is the shortest path from the start to the beginning of the cheat, plus the manhattan distance (walking through any walls), plus the shortest path from the end of the cheat to the end of the maze. So `cheat_cost(U, V) = shortest_path_cost(S, U) + manhattan_distance(U, V) + shortest_path_cost(V, E)`.

My solution's sluggish: 2 minutes for first part, 10 minutes for the second.

I tried a quirky way to get shortest path costs using tabling, and am not sure of its efficiency.