def find_S(grid):
    for j, line in enumerate(grid):
        try:
            return (j, line.index('S'))
        except ValueError:
            pass
    return None


def in_bounds(j, i, grid):
    return 0 <= j < len(grid) and 0 <= i < len(grid[0])


def display(level, grid):
    for j, line in enumerate(grid):
        sline = []
        for i, ch in enumerate(line):
            if (j, i) in level:
                sline.append('O')
            else:
                sline.append(ch)
        print(''.join(sline))


def run(fname, steps):
    grid = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            grid.append(line)
    S = find_S(grid)
    grid[S[1]] = grid[S[1]].replace('S', '.')
    level = { S }
    for i in range(steps):
        new_level = set()
        for (j, i) in level:
            for (dj, di) in (-1, 0), (1, 0), (0, 1), (0, -1):
                aj = j + dj
                ai = i + di
                if in_bounds(aj, ai, grid) and grid[aj][ai] == '.':
                    new_level.add((aj, ai))
        level = new_level
    print(len(level))


if __name__ == '__main__':
    run('input.txt', 64)