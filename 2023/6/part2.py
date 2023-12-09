def dist(t, h):
    return h * (t - h)

def parse_line(line):
    toks = iter(line.strip().split())
    next(toks)
    return int(''.join(toks))

def run():
    with open('input.txt') as f:
        lines = iter(f)
        t = parse_line(next(lines))
        record = parse_line(next(lines))
    result = sum(int(dist(t, hold) > record)
                 for hold in range(0, t+1))
    print(result)

if __name__ == '__main__':
    run()