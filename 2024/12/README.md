# Day 12

Solutions for both Part 1 and 2 are in `part1and2.pl`.

## Part 1

My initial approach was to implement a BFS in Prolog (`part1-bfs-abandoned.pl`), but wanted to push myself to write the code more declaratively. The idea I came up with was to model the problem with constraints.

* Associate each grid tile is associated with a partition ID, initially a free variable.
* Constrain adjacent tiles to have the same partition ID iff they are adjacent.

The implementation worked as specified, but it would assign the same IDs to two different blobs as long as they didn't touch (graph coloring). I wanted blobs to have globally unique IDs.

I hacked a workaround at the cost of purity, with the help of the `term_variables` builtin.

## Part 2

For the second part I found it simplest to count the number of corners instead of counting the number of edges (I think it comes out to the same thing). Based on some scribbling on paper I figured out how to determine a tile's number of corners based on its 8 adjacent tiles.