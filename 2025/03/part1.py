# O(n) time.

def parse():
    res = []
    for line in open('input.txt'):
        digits = [int(char) for char in line.strip()]
        res.append(digits)
    return res


def bank_joltage(digits):
    maxes = digits[:]
    i = len(digits)-2
    while i >= 0:
        maxes[i] = max(maxes[i], maxes[i+1])
        i -= 1
    # now maxes[i] contains the max of digits across indices >= i.

    maxjoltage = 0
    for i, d in enumerate(digits[:-1]):
        maxjoltage = max(maxjoltage, 10*d + maxes[i+1])
    return maxjoltage


def run():
    sum = 0
    for digits in parse():
        sum += bank_joltage(digits)
    print(sum)

if __name__ == '__main__':
    run()