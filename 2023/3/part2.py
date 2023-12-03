import re


def readlines():
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            yield line


def myfind(j, i, parsed):
    for num, (sj, si), (ej, ei) in parsed:
        if (sj <= j <= ej) and (si <= i < ei):
            return num, (sj, si), (ej, ei)
    return None


def solution(lines):
    s = 0
    parsed = parseNums(lines)
    n = len(lines[0])
    for j in range(len(lines)):
        for i in range(n):
            ch = lines[j][i]
            if ch != '*':
                continue
            # it's a gear
            adj = [
                (i-1, j-1), (i-1, j), (i-1, j+1),
                (i, j-1), (i, j+1),
                (i+1, j-1), (i+1, j), (i+1, j+1)
            ]
            distinct = set()
            for ni, nj in adj:
                if ni < 0 or ni >= n:
                    continue
                if nj < 0 or nj >= len(lines):
                    continue
                got = myfind(nj, ni, parsed)
                if got is not None:
                    distinct.add(got)
            if len(distinct) == 2:
                a, b = list(distinct)
                s += a[0]*b[0]
    return s


def parseNums(lines):
    parsed = []
    start = None
    end = None
    num = ''
    for j in range(len(lines)):
        for i in range(len(lines[j])):
            if '0' <= lines[j][i] <= '9':
                if num == '':
                    start = (j, i)
                num += lines[j][i]
            else:
                end = (j, i)
                if num != '':
                    parsed.append((int(num), start, end))
                num = ''
                start = None
                end = None
    return parsed



def run():
    assert parseNums(['123..424.12.']) == [(123, (0,0), (0,3)), (424, (0, 5), (0, 8)), (12, (0, 9), (0, 11))]
    lines = list(readlines())
    lines = [l+'.' for l in lines]
    print(solution(lines))


if __name__ == '__main__':
    print("running")
    run()