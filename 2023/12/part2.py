import functools


@functools.lru_cache(maxsize=None)
def arrangements(line, groups, require_next_dot, required_pounds):
    if len(line) == 0:
        if len(groups) == 0 and required_pounds == 0:
            return 1
        else:
            return 0
    head, tail = line[0], line[1:]
    match (require_next_dot, required_pounds > 0, head):
        case (True, False, '.' | '?'):
            return arrangements(tail, groups, False, 0)
        case (False, True, '#' | '?'):
            return arrangements(tail, groups, required_pounds == 1, required_pounds - 1)
        case (False, False, _):
            s = 0
            if head in '?.':
                s += arrangements(tail, groups, False, 0)
            if head in '?#' and len(groups) > 0:
                ghead, gtail = groups[0], groups[1:]
                s += arrangements(tail, gtail, ghead == 1, ghead-1)
            return s
        case _:
            return 0


def unfold_records(line, groups):
    return '?'.join(5*[line]), 5*groups


def run():
    s = 0
    with open('input.txt') as f:
        for i, line in enumerate(f):
            line = line.strip()
            line, groups = line.split()
            groups = tuple(map(int, groups.split(',')))
            line, groups = unfold_records(line, groups)
            v = arrangements(line, groups, False, 0)
            arrangements.cache_clear()
            print('processed line {}: {}'.format(i, v))
            s += v
    print(s)


if __name__ == '__main__':
    run()
