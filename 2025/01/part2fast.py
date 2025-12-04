# list of positive or negative turns.
def parse():
    with open('input.txt') as f:
        data = f.read()
    parsed = []
    for line in data.splitlines():
        line = line.strip()
        if not line:
            continue
        d = line[0]
        n = int(line[1:])
        n *= {'L': -1, 'R': 1}[d]
        parsed.append(n)
    return parsed


# the number of zeros when turn is positive is (a + turn) // 100
# when the turn is negative it is handled as if the position of the knob is mirrored
# so that 3 becomes 97, and the turn is in a positive direction.
def count_zeros(a, d):
    if d < 0:
        # 0 handled specially so that 0 mirrors to 0 instead of 100.
        if a != 0:
            a = 100 - a
        d *= -1
    return (a + d) // 100


def run():
    zeros = 0
    a = 50
    for d in parse():
        zeros += count_zeros(a, d)
        a = (a + d) % 100
    print(zeros)


if __name__ == '__main__':
    run()