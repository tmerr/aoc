'''AoC'''


numbersA = {
    'zero': '0',
    'one': '1',
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'nine': '9',
}

numbersB = {k[::-1]: v for k, v in numbersA.items()}

def rewrite(numbers, txt):
    res = ''
    i = 0
    while i < len(txt):
        matched = False
        for w, j in numbers.items():
            if txt[i:i+len(w)] == w:
                res += j
                i += len(w)
                matched = True
                break
        if not matched:
            res += txt[i]
            i += 1
    return res


def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            line1 = rewrite(numbersA, line)
            ch = [c for c in line1 if '0' <= c <= '9']
            line2 = rewrite(numbersB, line[::-1])
            ch2 = [c2 for c2 in line2 if '0' <= c2 <= '9']
            s += int(ch[0] + ch2[0])
    print(s)


if __name__ == '__main__':
    run()
