def readlines():
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            yield line


def adjIx(lines):
    n = len(lines[0])
    adjnums = []
    for j in range(len(lines)):
        for i in range(n):
            ch = lines[j][i]
            if '0' <= ch <= '9' or ch == '.':
                continue
            # it's a symbol
            adj = [
                (i-1, j-1), (i-1, j), (i-1, j+1),
                (i, j-1), (i, j+1),
                (i+1, j-1), (i+1, j), (i+1, j+1)
            ]
            for ni, nj in adj:
                if ni < 0 or ni >= n:
                    continue
                if nj < 0 or nj >= len(lines):
                    continue
                if '0' <= lines[nj][ni] <= '9':
                    adjnums.append((nj, ni))
    return adjnums


def run():
    tst = [
        '467..114..',
        '...*......',
        '..35..633.'
    ]
    #assert adjIx(tst) == [(0, 2), (2, 2), (2, 3)]
    lines = list(readlines())
    lines = [l+'.' for l in lines]
    aix = adjIx(lines)
    n = len(lines[0])
    num = ''
    keep = False
    s = 0
    emitted = 0
    for j in range(len(lines)):
        for i in range(n):
            if '0' <= lines[j][i] <= '9':
                num += lines[j][i]
            else:
                if num != '':
                    if keep:
                        s += int(num)
                    keep = False
                    num = ''
                    emitted += 1
            if (j, i) in aix:
                keep = True
    print(emitted)
    print(s)


if __name__ == '__main__':
    print("running")
    run()