def sign(num):
    if num == 0:
        return 0
    elif num > 0:
        return 1
    else:
        return -1


def is_safe(full_report):
    for skip_i in range(len(full_report)):
        report = full_report[:skip_i] + full_report[skip_i+1:]
        pairs = zip(report, report[1:])
        deltas = [right - left for left, right in pairs]
        signs = set(map(sign, deltas))
        signs_ok = signs == {1} or signs == {-1}
        deltas_ok = all(1 <= abs(d) <= 3 for d in deltas)
        if signs_ok and deltas_ok:
            return True
    return False


def run():
    reports = []
    with open('input.txt', 'r') as f:
        for line in f:
            report = line.strip().split()
            report = list(map(int, report))
            reports.append(report) 
    num_safe = 0
    for report in reports:
        if is_safe(report):
            num_safe += 1
    print(num_safe) 


if __name__ == '__main__':
    run()