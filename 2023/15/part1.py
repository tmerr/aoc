def hash(s):
    cur = 0
    for ch in s:
        cur += ord(ch)
        cur *= 17
        cur %= 256
    return cur


def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            line = line.replace('\n', '')
            steps = line.split(',')
            s += sum(map(hash, steps))
    print(s)


if __name__ == '__main__':
    run()