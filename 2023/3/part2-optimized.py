# Part 2 solution built on part1-optimized.
# Also O(filesize) time and O(line length) space.

import re


def readfile(fname):
    # readfile is a line iterator that also pads with a first and
    # last line of all dots, for example.
    # .................
    # <actual contents>
    # .................
    # this has two benefits. (1) it gets rid of some index
    # error edge cases by ensuring all positions adjacent
    # to a symbol are within bounds. and (2) it ensures there
    # is always a 3-line window that the algorithm can focus
    # on.
    # for convenience, the first yielded item is the line length.
    with open(fname) as f:
        dummy_line = None
        for i, line in enumerate(f):
            line = line.strip()
            if i == 0:
                dummy_line = '.' * len(line)
                yield len(line)
                yield dummy_line
            yield line
        yield dummy_line


class ParsedLine:
    def __init__(self, symbols, number_lookup):
        self.symbols = symbols
        self.number_lookup = number_lookup


class Entry:
    def __init__(self, num):
        self.num = num


def parse(line):
    number_lookup = [None]*len(line)
    symbols = []
    for m in re.finditer(r'\d+', line):
        start, end = m.span()
        entry = Entry(int(m.group(0)))
        for i in range(start, end):
            number_lookup[i] = entry
    for i, ch in enumerate(line):
        if ch == '*':
            symbols.append(i)
    return ParsedLine(symbols, number_lookup)


def solution(fname):
    s = 0
    it = iter(readfile(fname))
    line_length = next(it)
    window = [
        parse(next(it)),
        parse(next(it)),
        parse(next(it)),
    ]
    while True:
        for i in window[1].symbols:
            adj = [
                (i-1, 0), (i-1, 1), (i-1, 2),
                (i, 0), (i, 2),
                (i+1, 0), (i+1, 1), (i+1, 2)
            ]
            seen = set()
            for ni, nj in adj:
                if ni < 0 or ni >= line_length:
                    continue
                entry = window[nj].number_lookup[ni]
                if entry is None:
                    continue
                if entry not in seen:
                    seen.add(entry)
            if len(seen) == 2:
                a, b = seen
                s += a.num * b.num
        try:
            line = next(it)
        except StopIteration:
            break
        window[0] = window[1]
        window[1] = window[2]
        window[2] = parse(line)
    return s


def run():
    print(solution('input.txt'))


if __name__ == '__main__':
    run()