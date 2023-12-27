# Solution that relies on some coincidence in the input.

def lines():
    data = [
        '242565161831299, 288585065941956, 286974072498899 @ 47, 17, 109',
        '254508970226775, 221720800680738, 176771990266357 @ -91, 76, 30',
    ]
    res = []
    for line in data:
        a, b = line.split('@')
        pos = list(map(int, a.strip().split(', ')))
        vel = list(map(int, b.strip().split(', ')))
        res.append((pos, vel))
    return res


def collision_time(x0, vx, other_x0, other_vx):
    return (x0 - other_x0)/(other_vx - vx)


def run():
    # There are three lines in the input that have the exact same initial x and velocity x.
    # This only feels possible if
    # - The stone intercepts them at the exact same time (seems unlikely)
    # - Or, the stone has the exact same initial x and velocity.
    # I guess that it's the latter and hard code it.
    x0 = 229429688799267
    vx = 63
    collisions = []
    # That information is enough to find time of collision for other lines in the input.
    # Then go from time of collision -> points of collision.
    # And then 2 points of collision -> the line of the stone's position as a function of time.
    for (other_x0, other_y0, other_z0), (other_vx, other_vy, other_vz) in lines():
        t = collision_time(x0, vx, other_x0, other_vx)
        x = other_x0 + other_vx * t
        y = other_y0 + other_vy * t
        z = other_z0 + other_vz * t
        collisions.append((x, y, z, t))
    (x1, y1, z1, t1), (x2, y2, z2, t2)  = collisions
    slope_x = (x2 - x1)/(t2 - t1)
    slope_y = (y2 - y1)/(t2 - t1)
    slope_z = (z2 - z1)/(t2 - t1)
    x_intercept = x1 - slope_x * t1
    y_intercept = y1 - slope_y * t1
    z_intercept = z1 - slope_z * t1
    print('x, y, z: ', x_intercept, y_intercept, z_intercept)
    print(round(sum([x_intercept, y_intercept, z_intercept])))


if __name__ == '__main__':
    run()