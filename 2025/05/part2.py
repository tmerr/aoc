def run():
    fresh_ranges = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if '-' in line:
                start, end = line.split('-')
                fresh_ranges.append((int(start), int(end)))

    fresh_ranges.sort()
    merged_size = 0
    min_start = None
    max_end = None
    for i, (s, e) in enumerate(fresh_ranges):
        if min_start is None:
            min_start = s

        if max_end is None or e > max_end:
            max_end = e

        try:
            peek_s = fresh_ranges[i+1][0]
            if e >= peek_s:
                continue
        except IndexError:
            pass

        merged_size += max_end - min_start + 1
        min_start = None
        max_end = None

    print(merged_size)

if __name__ == '__main__':
    run()