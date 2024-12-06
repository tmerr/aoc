from collections import Counter


def run():
    xs = []
    ys = []
    with open('input.txt', 'r') as f:
        for line in f:
            x, y = line.strip().split()
            xs.append(int(x))
            ys.append(int(y))
    total = 0
    freq = Counter(ys)
    for x in xs:
        total += x * freq[x]
    print(total)


if __name__ == '__main__':
    run()