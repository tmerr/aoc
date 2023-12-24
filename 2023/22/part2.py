from itertools import product


class Brick:
    def __init__(self, cubes):
        self.cubes = cubes
    
    def min_z(self):
        return min(z for (_, _, z) in self.cubes)

    def clone(self):
        return Brick(self.cubes[:])


class BrickWorld:
    def __init__(self, bricks):
        self.bricks = bricks
        self.coord_to_brick = {}
        self.on_brick_fell = None
        for b in self.bricks:
            for cube in b.cubes:
                self.coord_to_brick[cube] = b

    def can_move_down(self, brick):
        for (x, y, z) in brick.cubes:
            if z == 1:
                # ground is below.
                return False
            other = self.coord_to_brick.get((x, y, z-1), None)
            if (other is not None) and (other is not brick):
                # collision.
                return False
        return True

    def move_down(self, brick):
        for cube in brick.cubes:
            del self.coord_to_brick[cube]
        for i in range(len(brick.cubes)):
            (x, y, z) = brick.cubes[i]
            brick.cubes[i] = (x, y, z-1)
        for cube in brick.cubes:
            self.coord_to_brick[cube] = brick

    def gravitate(self):
        while True:
            self.bricks.sort(key=Brick.min_z)
            changed = False
            for brick in self.bricks:
                if self.can_move_down(brick): 
                    self.move_down(brick)
                    if self.on_brick_fell is not None:
                        self.on_brick_fell(brick)
                    changed = True
            if not changed:
                break

    def bricks_supporting(self, brick):
        s = set()
        for (x, y, z) in brick.cubes:
            other = self.coord_to_brick.get((x, y, z-1), None)
            if (other is not None) and (other is not brick):
                s.add(other)
        return s

    def unsafe_bricks(self):
        res = set()
        for brick in self.bricks:
            supp = self.bricks_supporting(brick)
            if len(supp) == 1:
                res.update(supp)
        return res

    def with_brick_removed(self, brick):
        return BrickWorld([b.clone() for b in self.bricks if b != brick])


def run(fname):
    bricks = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            a, b = line.split('~')
            ax, ay, az = map(int, a.split(','))
            bx, by, bz = map(int, b.split(','))
            coords = list(product(
                range(ax, bx+1),
                range(ay, by+1),
                range(az, bz+1)
            ))
            bricks.append(Brick(coords))
    world = BrickWorld(bricks)
    world.gravitate()
    s = 0
    for u in world.unsafe_bricks():
        wclone = world.with_brick_removed(u)
        fell_set = set()
        wclone.on_brick_fell = fell_set.add
        wclone.gravitate()
        s += len(fell_set)
    print(s)



if __name__ == '__main__':
    run('input.txt')