from itertools import product


def substitute(line, assignment):
    assignment = {i: '#' if b else '.' for (i, b) in assignment}
    res = []
    for i, ch in enumerate(line):
        res.append(assignment.get(i, ch))
    return ''.join(res)


def expected_groups(arrangement):
    res = []
    cur = None
    for ch in arrangement:
        if ch == '#':
            if cur is None:
                cur = 1
            else:
                cur += 1
        elif cur is not None:
            res.append(cur)
            cur = None
    if cur is not None:
        res.append(cur)
        cur = None
    return res


def arrangements(line):
    line, groups = line.split()
    groups = list(map(int, groups.split(',')))
    unknowns = []
    for i, ch in enumerate(line):
        if ch == '?':
            unknowns.append([(i, True), (i, False)])
    assignments = product(*unknowns)
    arrangements = [substitute(line, a) for a in assignments]
    arrangements = filter(lambda a: expected_groups(a) == groups, arrangements)
    return arrangements


def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            s += len(list(arrangements(line)))
    print(s)


if __name__ == '__main__':
    run()
