def number_at(line, i):
    # returns the number and start offet.
    if not line[i].isnumeric():
        return None
    while True:
        prev = i-1
        if prev < 0 or not line[prev].isnumeric():
            break
        i -= 1
    start = i
    while i < len(line) and line[i].isnumeric():
        i += 1
    return int(line[start:i]), start


def solution(lines):
    s = 0
    for j, line in enumerate(lines):
        for i, ch in enumerate(line):
            if ch.isnumeric() or ch == '.':
                continue
            adj = [
                (i-1, j-1), (i-1, j), (i-1, j+1),
                (i, j-1), (i, j+1),
                (i+1, j-1), (i+1, j), (i+1, j+1)
            ]
            distinct = set()
            for ni, nj in adj:
                got = number_at(lines[nj], ni)
                if got is not None:
                    # consider numbers with the same value at different
                    # positions distinct.
                    distinct.add((j, got))
            for _, (n, _) in distinct:
                s += n
    return s


def run():
    with open('input.txt') as f:
        lines = f.read().strip().split()
    print(solution(lines))


if __name__ == '__main__':
    run()