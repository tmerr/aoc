def expand_rows(lines):
    res = []
    for line in lines:
        if all(ch == '.' for ch in line):
            res.append(line)
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


def shortest_path_lengths(lines, start, gs):
    lengths = []
    seen = set()
    want_to_see = set(gs)
    q = [(start, 0)]
    while True:
        u, d = q.pop(0)
        if u in want_to_see:
            want_to_see.remove(u)
            lengths.append(d)
        if len(want_to_see) == 0:
            break
        for dv in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            vj = u[0] + dv[0]
            vi = u[1] + dv[1]
            v = (vj, vi)
            if v in seen:
                continue
            seen.add(v)
            if 0 <= vj <= len(lines) and 0 <= vi <= len(lines[0]):
                q.append(((vj, vi), d+1))
    return lengths


def run():
    lines = []
    with open(0) as f:
        for line in f:
            lines.append(line.strip())
    lines = expand_rows(lines)
    lines = expand_cols(lines)
    gs = galaxy_pos(lines)
    s = 0
    for i, g in enumerate(gs):
        s += sum(shortest_path_lengths(lines, g, gs[i+1:]))
    print(s)


if __name__ == '__main__':
    run()