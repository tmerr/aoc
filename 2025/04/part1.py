from itertools import product


def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if line:
                grid.append(line)

    liftable = 0
    height = len(grid)
    width = len(grid[0])
    for j in range(height):
        for i in range(width):
            if grid[j][i] != '@':
                continue

            num_adj = 0
            for dj, di in product([-1, 0, 1], [-1, 0, 1]):
                if (dj, di) == (0, 0):
                    continue
                oj, oi = j+dj, i+di
                if 0 <= oj < height and 0 <= oi < width:
                    if grid[oj][oi] == '@':
                        num_adj += 1

            if num_adj < 4:
                liftable += 1

    print(liftable)


if __name__ == '__main__':
    run()