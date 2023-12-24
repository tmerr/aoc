from PIL import Image, ImageFont, ImageDraw

FONT_PATH = 'C:\\Windows\\Fonts\\arial.ttf'


def find_S(grid):
    for j, line in enumerate(grid):
        try:
            return (j, line.index('S'))
        except ValueError:
            pass
    return None


def lookup(j, i, grid):
    return grid[j % len(grid)][i % len(grid[0])]


def diamond_pos(a, b, shifted, og_grid_size):
    if shifted:
        sj = og_grid_size - 1
        si = (og_grid_size-1)//2 + 1
    else:
        sj = (og_grid_size-1)//2
        si = 0
    j = sj + a*og_grid_size
    i = si + b*og_grid_size
    return j, i


def diamond_area(startj, starti, shifted, og_grid_size):
    sza = (og_grid_size+1)//2
    szb = sza
    if shifted:
        sza -= 1
    avec = (-1, 1)
    bvec = (1, 1)
    for a in range(sza):
        for b in range(szb):
            j = startj + a*avec[0] + b*bvec[0]
            i = starti + a*avec[1] + b*bvec[1]
            yield (j, i)
            if not shifted and b != szb - 1 and a != sza - 1:
                yield (j, i+1)
    if shifted:
        startj += 1
        sza = (og_grid_size+1)//2 + 1
        szb = sza
        for a in range(sza-1):
            for b in range(szb-2):
                j = startj + a*avec[0] + b*bvec[0]
                i = starti + a*avec[1] + b*bvec[1]
                yield (j, i)

def pil_display(ix, level, grid):
    img = Image.new(mode='RGB', size=(len(grid[0]), len(grid)))
    draw = ImageDraw.Draw(img)
    font = ImageFont.truetype(FONT_PATH, 26)
    texts = []
    for (a, b, shifted) in [
        (0, 2, False),
        (0, 1, True), (0, 2, True),
        (1, 1, False), (1, 2, False), (1, 3, False),
        (1, 0, True), (1, 1, True), (1, 2, True), (1, 3, True),
        (2, 0, False), (2, 1, False), (2, 2, False), (2, 3, False), (2, 4, False),
        (2, 0, True), (2, 1, True), (2, 2, True), (2, 3, True),
        (3, 1, False), (3, 2, False), (3, 3, False),
        (3, 1, True), (3, 2, True),
        (4, 2, False),
    ]:
        rgb = (60*(a+1), 60*(b+1), 60 if shifted else 0)
        count = 0
        pos = diamond_pos(a, b, shifted, 131)
        for (j, i) in diamond_area(*pos, shifted, 131):
            if (j, i) in level:
                count += 1
            rgb2 = img.getpixel((i, j))
            rgb3 = ((rgb[0] + rgb2[0])//2, (rgb[1] + rgb2[1])//2, (rgb[2] + rgb2[2])//2)
            img.putpixel((i, j), rgb3)
        texts.append(((pos[1]+50, pos[0]), str(count)))
    for j, line in enumerate(grid):
        for i, ch in enumerate(line):
            if (j, i) in level:
                img.putpixel((i, j), (255, 255, 255))
    for (x, y), text in texts:
        draw.text((x, y), text, (255, 0, 0), font=font)
    img.save('animation/{}.png'.format(ix))
    img.close()


def parse(fname):
    grid = []
    with open(fname) as f:
        for line in f:
            line = line.strip()
            grid.append(line)
    S = find_S(grid)
    grid[S[1]] = grid[S[1]].replace('S', '.')
    return S, grid


def area(S, grid, steps):
    level = { S }
    for s in range(steps):
        new_level = set()
        for (j, i) in level:
            for (dj, di) in (-1, 0), (1, 0), (0, 1), (0, -1):
                aj = j + dj
                ai = i + di
                if lookup(aj, ai, grid) == '.':
                    new_level.add((aj, ai))
        level = new_level
        pil_display(s+1, level, grid)
        print('steps: {}'.format(s+1))
    return len(level)


def multiply_grid(grid, n):
    res = []
    for line in n*grid:
        res.append(n*line)
    return res


if __name__ == '__main__':
    S, grid = parse('input.txt')
    grid = multiply_grid(grid, 5)
    S = (len(grid)//2, len(grid[0])//2)
    area(S, grid, 65 + 2*131)