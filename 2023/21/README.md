## Part 1

Part 1 is a BFS problem. My implementation uses a list for each level instead of a queue since it makes the code simpler.

## Part 2

A brute force approach won't work for part 2 because of the high number of steps. Even if I came up with an algorithm that only took 131*131 iterations (grid size) per step, that feels like too many iterations total: ~half a trillion. I couldn't think of a fast enough general algorithm, so found a solution specific to the input. The key insights are

1. At 65 steps the reachable locations form a clean looking diamond. At multiples of 131 steps after that (the grid width & height) it encompasses a larger diamond area. This is relevant to the problem since the requested number of steps is exactly 65 + k*131 for some integer k.
1. Already reached locations have predictable behavior. Their state cycles with a period of 2.

Together this makes the solution sort of predictable. I took a guess at the formula based on a visualization (see visualization.png) and was right.

```python
>>> R = (26501365-65)/131
>>> D = 2*(R+1)
>>> 3738*D + 3722*(D//2) + 3638*(D//2) + (3738+3722+3638+3598)*((D//2)**2)
601441063166538
```

Generating the visualization was difficult because of off-by-one errors when painting the area of each diamond.