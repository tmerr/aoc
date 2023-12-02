def readlines():
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            yield line


def parse(line):
    it = iter(line.split())
    next(it)
    game = int(next(it).strip(':'))
    cubes_colors = []
    while True:
        try:
            cubes = int(next(it))
            color = next(it).strip(',;')
            cubes_colors.append([cubes, color])
        except StopIteration:
            break
    return game, cubes_colors


maxes = {'blue': 14, 'green': 13, 'red': 12}


def points(line):
    game, cubes_colors = parse(line)
    for cubes, color in cubes_colors:
        if cubes > maxes[color]:
            return 0
    return game


def power(line):
    _, cubes_colors = parse(line)
    colmax = {}
    for cubes, color in cubes_colors:
        colmax[color] = max(colmax.get(color, 0), cubes)
    product = 1
    for v in colmax.values():
        product *= v
    return product


def run():
    assert 1 == points('Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green')
    assert 0 == points('Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red')
    assert 2 == points('Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue')
    assert 0 == points('Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red')
    assert 5 == points('Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green')
    lines = list(readlines())
    print('part 1:', sum(map(points, lines)))
    print('part 2:', sum(map(power, lines)))


if __name__ == '__main__':
    run()
