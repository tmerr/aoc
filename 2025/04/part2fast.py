from itertools import product


directions = [d for d in product([-1, 0, 1], [-1, 0, 1])
              if d != (0, 0)]


def find_liftable(grid, candidates):
    res = []
    affected_neighbors = set()

    for (j, i) in candidates:
        if grid[j][i] != '@':
            continue
        adj = []
        for dj, di in directions:
            oj, oi = j+dj, i+di
            try:
                if grid[oj][oi] == '@':
                    adj.append((oj, oi))
            except IndexError:
                # out of bounds.
                pass
        if len(adj) < 4:
            affected_neighbors.update(adj)
            res.append((j, i))

    return res, affected_neighbors


def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if line:
                grid.append(list(line))

    height = len(grid)
    width = len(grid[0])
    candidates = product(range(height), range(width))
    num_liftable = 0
    while True:
        liftable, affected = find_liftable(grid, candidates)
        if not liftable:
            break
        for (j, i) in liftable:
            grid[j][i] = '.'
            num_liftable += 1
        candidates = affected
    print(num_liftable)


if __name__ == '__main__':
    run()
