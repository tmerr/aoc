def hash(s):
    cur = 0
    for ch in s:
        cur += ord(ch)
        cur *= 17
        cur %= 256
    return cur


def parse(step):
    for i, ch in enumerate(step):
        match ch:
            case '-':
                return '-', step[:i]
            case '=':
                return '=', step[:i], int(step[i+1:])
    return None


def find_lens(box, label):
    for i, (lbl, _) in enumerate(box):
        if lbl == label:
            return i
    return None


def run():
    boxes = [[] for _ in range(256)]
    with open('input.txt') as f:
        line = f.read().replace('\n', '')
        for step in line.split(','):
            match parse(step):
                case ('=', label, focal_length):
                    box = boxes[hash(label)]
                    ix = find_lens(box, label)
                    if ix is None:
                        box.append((label, focal_length))
                    else:
                        box[ix] = (label, focal_length)
                case ('-', label):
                    box = boxes[hash(label)]
                    ix = find_lens(box, label)
                    if ix is not None:
                        box.pop(ix)
                case _:
                    assert False
    power = 0
    for i, box in enumerate(boxes):
        for j, (_, focal_length) in enumerate(box):
            power += (i+1) * (j+1) * focal_length
    print(power)


if __name__ == '__main__':
    run()