def rotate_counterclockwise(grid):
    return tuple(zip(*grid))[::-1]


def roll(grid):
    res = [list(line[:]) for line in grid]
    for line in res:
        rollto = 0
        for i, ch in enumerate(line):
            match ch:
                case 'O':
                    line[i] = '.'
                    line[rollto] = 'O'
                    rollto += 1
                case '#':
                    rollto = i+1
                case '.':
                    pass
    return tuple(tuple(line) for line in res)


def cycle(grid):
    for i in range(4):
        grid = roll(grid)
        grid = rotate_counterclockwise(grid)
    return grid


def print_debug(grid):
    print('\n'.join(''.join(x) for x in zip(*grid)))
    print('')


def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            grid.append(list(line))
    # Transpose grid since it's more natural to operate within a row.
    # Now our job is to roll rocks east.
    grid = tuple(map(tuple, zip(*grid)))
    seen = {}
    CYCLES = 10**9
    i = 0
    looking_for_cycle = True
    while i < CYCLES:
        if looking_for_cycle:
            if grid in seen:
                j = seen[grid]
                cycle_length = i - j
                skip = cycle_length * ((CYCLES - i) // cycle_length)
                i += skip
                looking_for_cycle = False
                continue
            else:
                seen[grid] = i
        grid = cycle(grid)
        i += 1
    total_load = 0
    for line in grid:
        for i, ch in enumerate(line):
            if ch == 'O':
                total_load += len(line) - i
    print(total_load)


def run_tests():
    arg = ((0, 1),
           (2, 3))
    want = ((1, 3),
            (0, 2))
    assert rotate_counterclockwise(arg) == want


if __name__ == '__main__':
    run_tests()
    run()