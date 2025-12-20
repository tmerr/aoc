import operator

# transpose.
# example:
#   123 111
#   345 22
#   +   *
# becomes:
#   13+
#   24
#   35
#
#   12*
#   12
#   1
def transpose(rows):
    numrows = len(rows)
    numcols = len(rows[0])
    cols = [[None for i in range(numrows)] for j in range(numcols)]
    for i in range(numrows):
        for j in range(numcols):
            cols[j][i] = rows[i][j]
    return '\n'.join([''.join(col) for col in cols])


def run():
    with open('input.txt') as f:
        lines = [x for x in f]
    total = 0
    acc = None
    op = None
    for line in transpose(lines).splitlines() + ['']:
        line = line.strip()
        if not line:
            total += acc
            acc = 0
            continue
        if line[-1] == '+':
            op = operator.add
            acc = 0
        if line[-1] == '*':
            op = operator.mul
            acc = 1
        acc = op(acc, int(line.strip('*+')))
    print(total)


if __name__ == '__main__':
    run()