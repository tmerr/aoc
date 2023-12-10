def find_start(lines):
    for j, line in enumerate(lines):
        for i, ch in enumerate(line):
            if ch == 'S':
                return j, i
    return None

valid_to = {
    'S': [(-1, 0), (1, 0), (0, -1), (0, 1)],
    'L': [(-1, 0), (0, 1)],
    '7': [(0, -1), (1, 0)],
    'J': [(0, -1), (-1, 0)],
    'F': [(0, 1), (1, 0)],
    '-': [(0, -1), (0, 1)],
    '|': [(1, 0), (-1, 0)],
    '.': []
}

valid_from = {}
for ch, tos in valid_to.items():
    valid_from[ch] = [(-j, -i) for (j, i) in tos]


def valid_neighbors(lines, v):
    j, i = v
    res = []
    ch = lines[j][i]
    for dj, di in valid_to[ch]:
        aj, ai = j+dj, i+di
        if aj < 0 or aj >= len(lines):
            continue
        if ai < 0 or ai >= len(lines[0]):
            continue
        if not (dj, di) in valid_from[lines[aj][ai]]:
            continue
        res.append((aj, ai))
    return res 


def run():
    lines = []
    with open('input.txt') as f:
        for line in f:
            lines.append(list(line.strip()))
    v = find_start(lines)
    neighbors = valid_neighbors(lines, v)
    # cut out the start node and find the
    # length of the path between its neighbors,
    # since it's a little harder to mess up.
    assert len(neighbors) == 2
    lines[v[0]][v[1]] = '.'
    start = neighbors[0]
    goal = neighbors[1]
    seen = {start}
    q = [(start, 0)]
    dist_to_goal = None
    while len(q) > 0:
        u, dist = q.pop()
        if u == goal:
            dist_to_goal = dist
            break
        for v in valid_neighbors(lines, u):
            if v in seen:
                continue
            seen.add(v)
            q.append((v, dist+1))
    print('start: {}'.format(start))
    print('goal: {}'.format(goal))
    print('depth: {}'.format(dist_to_goal))
    print('steps-away: {}'.format(int(round(dist_to_goal+2)/2)))


if __name__ == '__main__':
    run()