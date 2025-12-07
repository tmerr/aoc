def parse():
    res = []
    for line in open('input.txt'):
        digits = [int(char) for char in line.strip()]
        res.append(digits)
    return res


def joltage(n, digits):
    # max subsequence of length n starting at digit index >= i.
    def max_subsequence(n, i):
        if n == 1:
            return max(digits[i:])

        maxdi = None
        for di in range(i, len(digits)-n+1):
            if maxdi is None or digits[di] > digits[maxdi]:
                maxdi = di

        return digits[maxdi] * 10**(n-1) + max_subsequence(n-1, maxdi+1)

    return max_subsequence(n, 0)


def run():
    sum = 0
    for digits in parse():
        sum += joltage(12, digits)
    print(sum)


if __name__ == '__main__':
    run()