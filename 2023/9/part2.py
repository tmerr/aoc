def deltas(seq):
    ds = []
    for s1, s2 in zip(seq, seq[1:]):
        ds.append(s2 - s1)
    if any(d != 0 for d in ds):
        return [ds] + deltas(ds)
    else:
        return [ds]


def predict(xs):
    res = 0
    for ds in deltas(xs)[::-1][1:] + [xs]:
        res = ds[0] - res
    return res


def run():
    assert predict([0, 3, 6, 9, 12, 15]) == -3
    assert predict([1, 3, 6, 10, 15, 21, 28]) == 0
    assert predict([10, 13, 16, 21, 30, 45]) == 5
    s = 0
    with open('input.txt') as f:
        for line in f:
            xs = list(map(int, line.split()))
            s += predict(xs)
    print(s)


if __name__ == '__main__':
    run()