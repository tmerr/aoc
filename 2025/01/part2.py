# list of positive or negative turns.
def parse():
    with open('input.txt') as f:
        data = f.read()
    parsed = []
    for line in data.splitlines():
        line = line.strip()
        if not line:
            continue
        d = line[0]
        n = int(line[1:])
        n *= {'L': -1, 'R': 1}[d]
        parsed.append(n)
    return parsed


def run():
    zeros = 0
    acc = 50
    for n in parse():
        while n != 0:
            if n < 0:
                n += 1
                acc -= 1
            elif n > 0:
                n -= 1
                acc += 1
            acc %= 100
            if acc == 0:
                zeros += 1
    print(zeros)


if __name__ == '__main__':
    run()