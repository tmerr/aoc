from functools import cache

def parse():
    res = []
    for line in open('input.txt'):
        digits = [int(char) for char in line.strip()]
        res.append(digits)
    return res


def joltage(n, digits):

    # max subsequence of length n starting at digit index >= i.
    @cache
    def max_subsequence(n, i):
        if i >= len(digits):
            return float('-inf')

        if n == 1:
            return max(digits[i:])

        b = 10**(n-1)
        return max(d*b + max_subsequence(n-1, i+di+1)
                   for di, d in enumerate(digits[i:]))

    return max_subsequence(n, 0)


def run():
    sum = 0
    for digits in parse():
        sum += joltage(12, digits)
    print(sum)

if __name__ == '__main__':
    run()