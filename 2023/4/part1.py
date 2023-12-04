def parse(line):
    it = iter(line.strip().split())
    next(it)
    next(it)
    winning = set()
    while (tok := next(it)) != '|':
        winning.add(int(tok))
    have = set()
    for tok in it:
        have.add(int(tok))
    return winning, have


def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            winning, have = parse(line)
            matches = len(winning.intersection(have))
            if matches > 0:
                s += 2**(matches-1)
    print(s)


if __name__ == '__main__':
    run()