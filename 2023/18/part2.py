def parse(fname):
    cmds = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            toks = iter(line.split())
            next(toks)
            next(toks)
            hx = next(toks).strip('()#')
            n = int(hx[:5], 16)
            match int(hx[5], 16):
                case 0: # R
                    d = (1, 0)
                case 1: # D
                    d = (0, -1)
                case 2: # L
                    d = (-1, 0)
                case 3: # U
                    d = (0, 1)
            cmds.append((d[0], d[1], n))
    return cmds


def normal_rot_90(dx, dy):
    return -0.5*dy, 0.5*dx


def normal_rot_270(dx, dy):
    return 0.5*dy, -0.5*dx


def vertices(cmds, normal):
    res = []
    # x, y is always a point at the center of some box.
    x, y = 0, 0
    for (dx1, dy1, n), (dx2, dy2, _) in zip(cmds, cmds[1:] + [cmds[0]]):
        # dig dig dig.
        x += n*dx1
        y += n*dy1
        # perpendicular to the dig direction.
        nx1, ny1 = normal(dx1, dy1)
        # perpendicular to the next dig direction.
        nx2, ny2 = normal(dx2, dy2)
        # combined it points to a corner.
        res.append((x + nx1 + nx2,
                    y + ny1 + ny2))
    return res


def polygon_area(vs):
    res = 0
    # see shoelace formula wikipedia: this is the triangle formula
    # from that page. any of the listed approaches are plenty fast.
    for (x1, y1), (x2, y2) in zip(vs, vs[1:] + [vs[0]]):
        res += x1 * y2 - x2 * y1
    res /= 2
    return abs(res)


def run(fname):
    cmds = parse(fname)
    # the choice of normal func decides whether to trace the outer or
    # inner perimeter around the blocks we dig out. we only care for the
    # outer, but it's hard to tell which function to use. so compute both
    # and take the larger.
    a1 = polygon_area(vertices(cmds, normal_rot_90))
    a2 = polygon_area(vertices(cmds, normal_rot_270))
    print(round(max(a1, a2)))


if __name__ == '__main__':
    run('input.txt')