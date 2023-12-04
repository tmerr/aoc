# A linear time solution for part 1
#
# Explanation: If input sizes were huge & we cared about time, we
# would have to fix part1-revised.py execution time being quadratic with line
# length for some inputs. Consider the input
#   3333333333333333333333333333333333333333333333333333
#   ****************************************************
# For each star the original algorithm would scan through every '3' to find
# the full number.
#
# part1-linear-time.py removes this overhead w/ a lookup table, so that overall
# execution time is linear with file length.

import re


class Entry:
    def __init__(self, num):
        self.num = num
        self.seen = False


def parse(txt):
    # generates
    # - List of symbol coordinates.
    # - A lookup table of numbers. Each entry is
    #   (value, line no, start offset, seen marker).
    #   The seen marker is used by later logic to mark
    #   that a number has been counted.
    lines = txt.strip().split()
    lookup_table = [[None for ch in line] for line in lines]
    symbols = []
    for j, line in enumerate(lines):
        matches = re.finditer(r'\d+', line)
        for m in matches:
            start, end = m.span()
            entry = Entry(int(m.group(0)))
            for i in range(start, end):
                lookup_table[j][i] = entry
        for i, ch in enumerate(line):
            if ch.isnumeric() or ch == '.':
                continue
            symbols.append((j, i))
    return symbols, lookup_table


def solution(symbols, lookup_table):
    s = 0
    for (j, i) in symbols:
        adj = [
            (i-1, j-1), (i-1, j), (i-1, j+1),
            (i, j-1), (i, j+1),
            (i+1, j-1), (i+1, j), (i+1, j+1)
        ]
        for ni, nj in adj:
            if nj < 0 or nj >= len(lookup_table):
                continue
            if ni < 0 or ni >= len(lookup_table[0]):
                continue
            got = lookup_table[nj][ni]
            if got is None:
                continue
            if not got.seen:
                s += got.num
                got.seen = True
    return s


def run():
    with open('input.txt') as f:
        txt = f.read()
    symbols, lookup_table = parse(txt)
    print(solution(symbols, lookup_table))


if __name__ == '__main__':
    run()