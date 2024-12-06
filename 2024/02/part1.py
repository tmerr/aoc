def sign(num):
    if num == 0:
        return 0
    elif num > 0:
        return 1
    else:
        return -1


def run():
    reports = []
    with open('input.txt', 'r') as f:
        for line in f:
            report = line.strip().split()
            report = list(map(int, report))
            reports.append(report) 
    num_safe = 0
    for report in reports:
        pairs = zip(report, report[1:])
        deltas = [right - left for left, right in pairs]
        signs = set(map(sign, deltas))
        if not (signs == {1} or signs == {-1}):
            # failed the all increasing or all decreasing condition
            continue
        if all(1 <= abs(d) <= 3 for d in deltas):
            num_safe += 1
    print(num_safe) 


if __name__ == '__main__':
    run()