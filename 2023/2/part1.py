# import os


def readlines():
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            yield line


def is_possible(line):
    parts = line.split(':')[1].split(';')
    parts = ' '.join([p.strip().replace(',', '') for p in parts]).split()
    for i in range(len(parts)//2):
        x = int(parts[2*i])
        col = parts[2*i+1]
        if col == 'blue' and x > 14 or x < 0:
            return False
        if col == 'green' and x > 13 or x < 0:
            return False
        if col == 'red' and x > 12 or x < 0:
            return False
    return True


def run():
    assert is_possible('Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green')
    assert not is_possible('Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red')
    assert is_possible('Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue')
    assert not is_possible('Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red')
    assert is_possible('Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green')
    s = 0
    for line in readlines():
        theid = line.split()[1].strip(':')
        if is_possible(line):
            s += int(theid)
    print(s)


if __name__ == '__main__':
    run()
