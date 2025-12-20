def run():
    fresh_ranges = []
    available = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            match line.split('-'):
                case [avail]:
                    available.append(int(avail))
                case [start, end]:
                    fresh_ranges.append((int(start), int(end)))
                case _:
                    raise ValueError('unrecognized line')

    count = 0
    for a in available:
        for start, end in fresh_ranges:
            if start <= a <= end:
                count += 1
                break
    print(count)

if __name__ == '__main__':
    run()