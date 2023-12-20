from functools import total_ordering
from itertools import product
import heapq


MIN_STEPS_BEFORE_TURN = 4
MAX_STEPS_IN_DIRECTION = 10


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
        # number of steps taken in a direction.
        # note 0 is unused except at start.
        range(0, MAX_STEPS_IN_DIRECTION+1),
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
    # Maximum of ten blocks without turning.
    if d < MAX_STEPS_IN_DIRECTION:
        append_if_in_bounds(adj, (j+fj, i+fi, d+1, fj, fi), grid)
    # Minimum of four blocks in the direction before turning.
    if d >= MIN_STEPS_BEFORE_TURN:
        # Left
        Lfj, Lfi = fi, -fj
        append_if_in_bounds(adj, (j+Lfj, i+Lfi, 1, Lfj, Lfi), grid)
        # Right
        Rfj, Rfi = -fi, fj
        append_if_in_bounds(adj, (j+Rfj, i+Rfi, 1, Rfj, Rfi), grid)
    def moved_four_before_end(adj):
        if (adj[0], adj[1]) != (len(grid)-1, len(grid[0])-1):
            return True
        return adj[2] >= MIN_STEPS_BEFORE_TURN
    return [states[a] for a in adj if moved_four_before_end(a)]


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
    best = (float('inf'), SearchState((0,)))
    for d in range(MAX_STEPS_IN_DIRECTION):
        for fj, fi in (-1, 0), (1, 0), (0, -1), (0, 1):
            s = states[(wantj, wanti, d, fj, fi)]
            best = min(best, (s.dist, s))
    path = [best[1]]
    while path[0].parent is not None:
        path.insert(0, path[0].parent)
    return best[0], [(s.p[0], s.p[1]) for s in path]

def run(fname, assert_best=None):
    grid = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            line = [int(x) for x in line]
            grid.append(line)
    best, path = shortest_path(grid)
    for j, line in enumerate(grid):
        xs = []
        for i, n in enumerate(line):
            if (j, i) in path:
                xs.append('.')
            else:
                xs.append(str(n))
        print(''.join(xs))
    print('best: ', best)
    if assert_best:
        assert best == assert_best
    print()


if __name__ == '__main__':
    run('testinput1.txt', assert_best=94)
    run('testinput2.txt', assert_best=71)
    run('input.txt')