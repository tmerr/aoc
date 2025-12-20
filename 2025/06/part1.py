def parse():
    columns = None
    ops = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            if line[0] in ['*', '+']:
                ops = line.split()
            else:
                data = line.split()
                if columns is None:
                    columns = [[int(d)] for d in data]
                else:
                    for i, d in enumerate(data):
                        columns[i].append(int(d))
    return columns, ops


def product(xs):
    res = 1
    for x in xs:
        res *= x
    return res


def run():
    columns, ops = parse()
    total = 0
    for column, op in zip(columns, ops):
        if op == '+':
            f = sum
        if op == '*':
            f = product
        total += f(column)
    print(total)


if __name__ == '__main__':
    run()