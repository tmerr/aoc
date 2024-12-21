# Day 4

## Part 1

Part 1 solution counts the number of (coordinates, direction) to spell
X M A S, rooted at X. For example 

```
. . . S . . .
. . . A . . .
. . . M . . .
. . . X M A S
```

Would have two solutions. One rooted at (X=3, Y=0) in direction (1, 0),
and the other rooted at (3, 0) in direction (0, -1).

## Part 2

Part 2 counts the number of tiles centered at A that have some rotation of

```
M M
 A
S S
```

## Properties

`part1.pl` and `part2.pl` are some of my first tries at Prolog. They
are readable (to me anyway), but on any other dimension they are bad programs.

* They're slow, taking 50 seconds and 120 seconds respectively on my system.
  The slowness comes from many O(file length) lookups of coordinates.
* They are not fun, since they can only run in the forward direction
  (given an input, produce the output). This caused by using "impure" Prolog
  features `aggregate_all`, `is`, and `\+`.

`part2-clp.pl` is a rework of `part2.pl`.

* This version is faster: about 7x faster, taking only 18 seconds. To do this I needed fast
  list lookups. `arraytree.pl` library implements O(log(n)) lookups of a fixed size list
  by converting the list to a binary tree. A simpler and faster alternative would be to
  use `assertz` to dynamically insert facts about coordinates into the Prolog database,
  but that would make the program less fun.
* This version is fun, since it can run in the reverse direction using the
  same code. Some of the tricks are from https://www.metalevel.at/prolog/purity.

For example I can ask Prolog "generate 4 by 4 boards that have a count of 2?"

```
?- X=4, Y=4, Count=2, problem(G, X, Y, Count).
Problem
AAAA
MMMM
AAAA
SSSS
```