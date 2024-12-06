def run():
    xs = []
    ys = []
    with open('input.txt', 'r') as f:
        for line in f:
            x, y = line.strip().split()
            xs.append(int(x))
            ys.append(int(y))
    total = 0
    for x, y in zip(sorted(xs), sorted(ys)):
        total += abs(x-y)
    print(total)


if __name__ == '__main__':
    run()