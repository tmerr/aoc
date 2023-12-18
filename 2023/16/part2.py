from dataclasses import dataclass
import sys

sys.setrecursionlimit(10000)


@dataclass(frozen=True)
class FrameOfReference:
    y: int
    x: int
    # unit vector: which way are we facing?
    dy: int
    dx: int

    def ahead(self, n):
        return FrameOfReference(self.y + n * self.dy,
                                self.x + n * self.dx,
                                self.dy,
                                self.dx)

    def react(self, tile):
        match tile:
            case '.':
                return [self]
            case '-' if abs(self.dx) == 1:
                return [self]
            case '|' if abs(self.dy) == 1:
                return [self]
            case '\\':
                return [FrameOfReference(self.y, self.x, self.dx, self.dy)]
            case '/':
                return [FrameOfReference(self.y, self.x, -self.dx, -self.dy)]
            case '-':
                return [FrameOfReference(self.y, self.x, 0, x) for x in [-1, 1]]
            case '|':
                return [FrameOfReference(self.y, self.x, y, 0) for y in [-1, 1]]
            case _:
                assert False


def grid_at(grid, frame):
    if 0 <= frame.y < len(grid) and 0 <= frame.x < len(grid[0]):
        return grid[frame.y][frame.x]
    raise IndexError()


def energized_positions(grid, start):
    visited = set()
    def energize(src):
        try:
            tile = grid_at(grid, src)
        except IndexError:
            return
        if src in visited:
            return
        visited.add(src)
        fs = src.react(tile)
        for f in fs:
            energize(f.ahead(1))
    energize(start)
    return len({(f.y, f.x) for f in visited})


def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            grid.append(line.strip())
    visited = set()
    def energize(src):
        try:
            tile = grid_at(grid, src)
        except IndexError:
            return
        if src in visited:
            return
        visited.add(src)
        fs = src.react(tile)
        for f in fs:
            energize(f.ahead(1))
    starts = []
    for y in range(len(grid)):
        starts.append(FrameOfReference(y, 0, 0, 1))
        starts.append(FrameOfReference(y, len(grid[0])-1, 0, -1))
    for x in range(len(grid[0])):
        starts.append(FrameOfReference(0, x, 1, 0))
        starts.append(FrameOfReference(len(grid)-1, x, -1, 0))
    print(max(energized_positions(grid, s) for s in starts))


if __name__ == '__main__':
    run()