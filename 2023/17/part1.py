from functools import total_ordering
from itertools import product
import heapq


# total ordering allows us to store this in the heap queue.
# the specific order is not important as long as it does
# not change.
@total_ordering
class SearchState:
    def __init__(self, p):
        self.dist = float('inf')
        self.parent = None
        self.finalized = False
        self.p = p

    def __eq__(self, other):
        if not hasattr(other, 'p'):
            return NotImplemented
        return self.p == other.p
    
    def __lt__(self, other):
        if not hasattr(other, 'p'):
            return NotImplemented
        return self.p < other.p


def state_space(grid):
    dimensions = [
        # row
        range(len(grid)),
        # col
        range(len(grid[0])),
        # depth. depth=2 means no more steps can be taken in the same direction.
        range(3),
        # facing row, col.
        [(-1, 0), (1, 0), (0, -1), (0, 1)],
    ]
    states = {}
    for (j, i, d, (fj, fi)) in product(*dimensions):
        p = (j, i, d, fj, fi)
        states[p] = SearchState(p)
    return states


def append_if_in_bounds(seq, v, grid):
    if 0 <= v[0] < len(grid) and 0 <= v[1] < len(grid[0]):
        seq.append(v)


def adjacent(s, grid, states):
    # Todo: Special case the first one?
    j, i, d, fj, fi = s.p
    adj = []
    # Forward
    if d < 2:
        append_if_in_bounds(adj, (j+fj, i+fi, d+1, fj, fi), grid)
    # Left
    Lfj, Lfi = fi, -fj
    append_if_in_bounds(adj, (j+Lfj, i+Lfi, 0, Lfj, Lfi), grid)
    # Right
    Rfj, Rfi = -fi, fj
    append_if_in_bounds(adj, (j+Rfj, i+Rfi, 0, Rfj, Rfi), grid)
    return [states[a] for a in adj]


def shortest_path(grid):
    # Hello dijkstra my old friend...
    states = state_space(grid)
    start = states[(0, 0, 0, 0, 1)]
    start.dist = 0
    frontier = [(start.dist, start)]
    while len(frontier) > 0:
        _, u = heapq.heappop(frontier)
        if u.finalized:
            # if enqueued multiple times then take only the first (least cost).
            continue
        u.finalized = True
        for v in adjacent(u, grid, states):
            dist = u.dist + grid[v.p[0]][v.p[1]]
            if dist < v.dist:
                v.dist = dist
                v.parent = u
                heapq.heappush(frontier, (v.dist, v))
    # We have a few shortest paths to the wanted (row, col) that only vary by
    # facing direction & depth (number of steps in one direction). Pick the
    # lowest.
    wantj = len(grid)-1
    wanti = len(grid[0])-1
    best = float('inf')
    for d in range(3):
        for fj, fi in (-1, 0), (1, 0), (0, -1), (0, 1):
            best = min(best, states[(wantj, wanti, d, fj, fi)].dist)
    return best

def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            line = [int(x) for x in line]
            grid.append(line)
    print(shortest_path(grid))

if __name__ == '__main__':
    run()