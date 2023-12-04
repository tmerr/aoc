# An O(file size) time, O(line length) space solution for part1.
#
# Each symbol can only cause only the current line, below, and above
# to influence the sum. So, we operate on a sliding window of 3 lines.
#
# Whenever we see a symbol, use a lookup table to find the integers
# adjacent to the symbol, where the lookup table is from
# position to some uniquely identified integer (an 'Entry' python
# object).

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
        # prevents double-counting a number.
        self.seen = False


def parse(line):
    number_lookup = [None]*len(line)
    symbols = []
    for m in re.finditer(r'\d+', line):
        start, end = m.span()
        entry = Entry(int(m.group(0)))
        for i in range(start, end):
            number_lookup[i] = entry
    for i, ch in enumerate(line):
        if ch.isnumeric() or ch == '.':
            continue
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
            for ni, nj in adj:
                if ni < 0 or ni >= line_length:
                    continue
                entry = window[nj].number_lookup[ni]
                if entry is None:
                    continue
                if not entry.seen:
                    s += entry.num
                    entry.seen = True
        try:
            line = next(it)
        except StopIteration:
            break
        window = [
            window[1],
            window[2],
            parse(line),
        ]
    return s


def run():
    print(solution('input.txt'))


if __name__ == '__main__':
    run()