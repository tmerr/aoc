import time

def factors(n):
    for i in range(1, (n//2)+1):
        if n % i == 0:
            yield i


# Splits up a (start, end) inclusive range into smaller ranges grouped by number of digits.
# Example: 3, 321 => (1, '3', '9'), (2, '10', '99'), (3, '100', '321')
def ranges_by_digits(start, end):
    for i in range(len(start), len(end)+1):
        if i == len(start):
            startn = start
        else:
            startn = '1' + '0'*(i-1)
        
        if i == len(end):
            endn = end
        else:
            endn = '9'*i

        yield (i, startn, endn)


def get_invalids(start, end):
    res = set()
    for i, startn, endn in ranges_by_digits(start, end):
        for chunksize in factors(i):
            numchunks = i // chunksize
            for chunk in range(int(startn[:chunksize]), int(endn[:chunksize])+1):
                candidate = int(str(chunk)*numchunks)
                if int(start) <= candidate <= int(end):
                    res.add(candidate)
    return res


def run():
    t0 = time.time()
    with open('input.txt') as f:
        data = f.read().strip()
    t1 = time.time()
    invalids = set()
    for pair in data.split(','):
        start, end = pair.split('-')
        invalids.update(get_invalids(start, end))
    res = sum(invalids)
    t2 = time.time()
    print(res)
    print(f'timing: file IO: {t1 - t0}s, processing: {t2 - t1}s')


if __name__ == '__main__':

    run()
    #print('seconds: {}'.format(time.time() - t0))