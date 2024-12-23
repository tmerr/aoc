# Day 6

## Part 1

Part 1 uses a visited set to mark locations.

## Part 2

Part 2 pseudocode:

```
count = 0
for each visited location from part 1:
  place a hypothetical obstruction.
  walk from the start. if a cycle is detected, increment count.
```

Cycle detection works by marking visited locations in a 3D grid of (X, Y, facing).

This one's slow, ~ 6 minutes on my machine.

## Implementation details

To implement the visited set, I reused the immutable grid from day 4 plus some tweaks.

* Support immutable O(log(n)) updates to positions with `replace_grid_tile`.
* Generalized the grid so it is N-dimensional.

I think a perfectly reasonable alternative would have been to use AVL trees or RB trees from the standard library, but I like *my* grid because it's *mine*.

After sorting out data structures I had to do a lot of performance tuning. Some findings:

  * CLPFD reification, e.g. `#<==>` is unusably slow within tight loops.
  * It is better to pass integers into CLPFD constraints than variables
    that are only later evaluated. The fastest code with CLFPD seems to be basically
    imperative code, since there is a fast path e.g. for `X #= Y + Z`. If Y and Z are integers then X is an integer.
  * To deal with stack overflows, try making predicates deterministic. This can apparently unlock tail-call optimization. I don't know how to check if I succeeded in making part 2 `walk` tail-call-optimized, but making predicates deterministic resolved my stack overflow issues one way or another.