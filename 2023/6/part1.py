def dist(t, h):
    return h * (t - h)

def run():
    with open('input.txt') as f:
        lines = iter(f)
        times = map(int, next(lines).strip().split()[1:])
        records = map(int, next(lines).strip().split()[1:])
    result = 1
    for t, record in zip(times, records):
        result *= sum(int(dist(t, hold) > record)
                      for hold in range(0, t+1))
    print(result)

if __name__ == '__main__':
    run()