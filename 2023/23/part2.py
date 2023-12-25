from pprint import pprint
from collections import deque, defaultdict
import sys

sys.setrecursionlimit(10000)

cache = {}
count = 0


def adjacency(grid):
    adj = defaultdict(list)
    for j, line in enumerate(grid):
        for i, ch in enumerate(line):
            if ch not in '.>v<^':
                continue
            for (dj, di) in (-1, 0), (1, 0), (0, -1), (0, 1):
                aj = j + dj
                ai = i + di
                if not (0 <= aj < len(grid) and 0 <= ai < len(grid[0])):
                    continue
                if grid[aj][ai] not in '.>v<^':
                    continue
                adj[(j, i)].append(((aj, ai), 1))
    return adj


def weighted(s, e, grid):
    adj = adjacency(grid)
    while True:
        changed = False
        for u, a in adj.items():
            if len(a) == 2 and u not in (s, e):
                # The vertices v - u - w are in a line.
                # Remove u, and connect v - w directly with larger
                # weight.
                v, vn = a[0]
                w, wn = a[1]
                adj[v] = [(x, n) for x, n in adj[v] if x != u]
                adj[w] = [(x, n) for x, n in adj[w] if x != u]
                del adj[u]
                adj[v].append((w, vn+wn))
                adj[w].append((v, vn+wn))
                changed = True
                break
        if not changed:
            break
    return adj


def longest_path(u, e, visited, adj):
    if u == e:
        return 0
    longest = float('-inf')
    for v, n in adj[u]:
        if v in visited:
            continue
        visited.add(v)
        longest = max(longest, n + longest_path(v, e, visited, adj))
        visited.remove(v)
    return longest


def start(grid):
    for i, ch in enumerate(grid[0]):
        if ch == '.':
            return (0, i)
    raise ValueError('unable to find start')


def end(grid):
    for i, ch in enumerate(grid[-1]):
        if ch == '.':
            return (len(grid)-1, i)
    raise ValueError('unable to find end')


def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            grid.append(line)
    S = start(grid)
    E = end(grid)
    # Preprocessing the graph into a weighted graph helps a lot w/
    # performance. This only has 36 nodes compared to the much larger
    # original.
    w = weighted(S, E, grid)
    print(longest_path(S, E, {S}, w))


if __name__ == '__main__':
    run()