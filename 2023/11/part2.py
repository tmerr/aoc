def expand_rows(lines):
    res = []
    for line in lines:
        if all(ch == '.' for ch in line):
            for i in range(1000000):
                res.append(line)
        else:
            res.append(line)
    return [''.join(row) for row in res]


def expand_cols(lines):
    return transpose(expand_rows(transpose(lines)))


def transpose(lines):
    res = [[] for _ in range(len(lines[0]))]
    for line in lines:
        for c, ch in enumerate(line):
            res[c].append(ch)
    return [''.join(row) for row in res]


def galaxy_pos(lines):
    res = []
    for r, row in enumerate(lines):
        for c, ch in enumerate(row):
            if ch != '.':
                res.append((r, c))
    return res


def manhattan_distance(u, v):
    return abs(u[0] - v[0]) + abs(u[1] - v[1])


def shortest_path_lengths(start, gs):
    lengths = []
    for g in gs:
        lengths.append(manhattan_distance(start, g))
    return lengths


def run():
    lines = []
    with open(0) as f:
        for line in f:
            lines.append(line.strip())
    # 1 million X 1 million grid would be too big,
    # so avoid considering expansion in both directions at once.
    # This could be made a lot more efficient using a different
    # approach, but was fast enough for the challenge.
    gs1 = galaxy_pos(expand_rows(lines))
    gs2 = galaxy_pos(expand_cols(lines))
    gs = [(aj, bi) for ((aj, ai), ((bj, bi))) in zip(gs1, gs2)]
    s = 0
    for i, g in enumerate(gs):
        s += sum(shortest_path_lengths(g, gs[i+1:]))
    print(s)


if __name__ == '__main__':
    run()