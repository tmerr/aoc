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


def enclosed_area(lines, path):
    height = len(lines)
    width = len(lines[0])
    # Polygon filling trick: Scan from left to right and count if we've
    # crossed an edge of the shape an even or odd number of times.
    # Whenever it's odd, we're inside.
    display = [[' ' for x in range(width)] for _ in range(height)]
    area = 0
    lookup = set(path)
    for j in range(height):
        filling = False
        for i in range(width):
            if (j, i) in lookup:
                display[j][i] = '*'
                ch = lines[j][i] 
                display[j][i] = ch
                # Corners tiles are funky, see these visuals of "L7" and "|".
                # *                            *
                # *                            *
                # *** ***   <-  L7             *     | <-
                #       *                      *
                #       *                      *
                # Depending how high the horizontal line for our counter is, it crosses
                # through just one of L or 7. We need to pick just one level and be consistent
                # for correct results. If we choose low then we have to count 7 and F and not
                # L, J. We could alternatively count L and J but not 7 and F.
                #
                # S is also funky. Its shape sort of depends on its surroundings.
                # In the input.txt it's effectively a 7, so it should be included IFF 7 is considered a
                # crossing.
                #
                # Put all together, the check could either be '|LJ' or '|F7S' with identical results.
                if ch in '|LJ':
                    filling = not filling
                continue
            if filling:
                display[j][i] = 'I'
                area += 1
    print('\n'.join(''.join(line) for line in display))
    return area


def run():
    lines = []
    with open('input.txt') as f:
        for line in f:
            lines.append(list(line.strip()))
    theS = find_start(lines)
    neighbors = valid_neighbors(lines, theS)
    # cut out the start node and find the
    # length of the path between its 2 neighbors.
    assert len(neighbors) == 2
    lines1 = [line[:] for line in lines]
    lines1[theS[0]][theS[1]] = '.'
    start = neighbors[0]
    goal = neighbors[1]
    seen = {start}
    q = [(start, 0)]
    parents = {}
    dist_to_goal = None
    while len(q) > 0:
        u, dist = q.pop(0)
        if u == goal:
            dist_to_goal = dist
            break
        for v in valid_neighbors(lines1, u):
            if v in seen:
                continue
            parents[v] = u
            seen.add(v)
            q.append((v, dist+1))
    print('start: {}'.format(start))
    print('goal: {}'.format(goal))
    print('depth: {}'.format(dist_to_goal))
    print('steps-away: {}'.format(int(round(dist_to_goal+2)/2)))
    path = [goal]
    while True:
        cur = path[-1]
        if cur not in parents:
            break
        path.append(parents[cur])
    path.append(theS)
    print('enclosed area: {}'.format(enclosed_area(lines, path)))



if __name__ == '__main__':
    run()