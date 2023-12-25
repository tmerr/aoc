import sys


sys.setrecursionlimit(10000)


def longest_path(path, e, grid):
    if path and path[-1] == e:
        return 0
    longest = float('-inf')
    for (dj, di) in (-1, 0), (1, 0), (0, -1), (0, 1):
        u = path[-1]
        aj = u[0] + dj
        ai = u[1] + di
        if not (0 <= aj < len(grid) and 0 <= ai < len(grid[0])):
            continue
        match (grid[aj][ai], (dj, di)):
            case '.', _:
                pass
            case '>', (0, 1):
                pass
            case 'v', (1, 0):
                pass
            case '<', (0, -1):
                pass
            case '^', (-1, 0):
                pass
            case _:
                continue
        v = (aj, ai)
        if v in path:
            continue
        path.append(v)
        longest = 1 + max(longest, longest_path(path, e, grid))
        path.pop()
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
    print(longest_path([S], E, grid))


if __name__ == '__main__':
    run()