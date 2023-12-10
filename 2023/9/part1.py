def deltas(seq):
    ds = []
    for s1, s2 in zip(seq, seq[1:]):
        ds.append(s2 - s1)
    if any(d != 0 for d in ds):
        return [ds] + deltas(ds)
    else:
        return [ds]


def predict(xs):
    return xs[-1] + sum(d[-1] for d in deltas(xs))


def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            xs = list(map(int, line.split()))
            s += predict(xs)
    print(s)


if __name__ == '__main__':
    run()