# import os


def readlines():
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            yield line


def is_possible(line):
    parts = line.split(':')[1].split(';')
    parts = ' '.join([p.strip().replace(',', '') for p in parts]).split()
    colmax = {}
    result = True
    for i in range(len(parts)//2):
        x = int(parts[2*i])
        col = parts[2*i+1]
        colmax[col] = max(colmax.get(col, 0), x)
        if col == 'blue' and x > 14 or x < 0:
            result = False
        if col == 'green' and x > 13 or x < 0:
            result = False
        if col == 'red' and x > 12 or x < 0:
            result = False
    power = 1
    for v in colmax.values():
        power *= v
    return result, power


def run():
    s = 0
    for line in readlines():
        theid = line.split()[1].strip(':')
        p, power = is_possible(line)
        s += power
    print(s)


if __name__ == '__main__':
    run()
