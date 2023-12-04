from collections import defaultdict


def parse(line):
    it = iter(line.strip().split())
    next(it)
    cardno = int(next(it).strip(':'))
    winning = set()
    while (tok := next(it)) != '|':
        winning.add(int(tok))
    have = set()
    for tok in it:
        have.add(int(tok))
    return cardno, winning, have


def run():
    count = 0
    instances = defaultdict(lambda: 1)
    with open('input.txt') as f:
        for line in f:
            cardno, winning, have = parse(line)
            inst = instances[cardno]
            count += inst
            match = len(winning.intersection(have))
            for c in range(cardno+1, cardno+1+match):
                instances[c] += inst
    print(count)


if __name__ == '__main__':
    run()