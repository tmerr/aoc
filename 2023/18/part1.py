from collections import deque

def parse(fname):
    cmds = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            toks = iter(line.split())
            match next(toks):
                case 'R':
                    d = (0, 1)
                case 'L':
                    d = (0, -1)
                case 'U':
                    d = (-1, 0)
                case 'D':
                    d = (1, 0)
                case _:
                    assert False
            n = int(next(toks))
            cmds.append((d[0], d[1], n))
    return cmds


def init_grid(cmds):
    # Choose starting coords and a grid such that all positions
    # we might move to are within bounds.
    minj, mini = 0, 0
    maxj, maxi = 0, 0
    j, i = 0, 0
    for (dj, di, n) in cmds:
        j += n * dj
        i += n * di
        minj = min(minj, j)
        mini = min(mini, i)
        maxj = max(maxj, j)
        maxi = max(maxi, i)
    height = maxj - minj + 1
    width = maxi - mini + 1
    grid = [[False for ii in range(width)] for jj in range(height)]
    return -minj, -mini, grid


def outline(j, i, grid, cmds):
    grid[j][i] = True
    for (dj, di, n) in cmds:
        for _ in range(n):
            j += dj
            i += di
            grid[j][i] = True


def flood_fill(sj, si, grid):
    q = deque([(sj, si)])
    while len(q) > 0:
        (j, i) = q.popleft()
        for (dj, di) in (-1, 0), (1, 0), (0, -1), (0, 1):
            aj = j + dj
            ai = i + di
            if 0 <= aj <= len(grid) and 0 <= ai <= len(grid[0]) and not grid[aj][ai]:
                grid[aj][ai] = True
                q.append((aj, ai))


def display_grid(grid):
    for line in grid:
        print(''.join('#' if v else '.' for v in line))


def run(fname):
    cmds = parse(fname)
    j, i, grid = init_grid(cmds)
    outline(j, i, grid, cmds)
    # j+1 and i+1 chosen by hand.
    flood_fill(j+1, i+1, grid)
    count = 0
    for line in grid:
        for v in line:
            if v:
                count += 1
    print(count)


if __name__ == '__main__':
    run('input.txt')