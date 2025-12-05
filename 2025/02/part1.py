def run():
    with open('input.txt') as f:
        data = f.read().strip()

    invalids = []
    for pair in data.split(','):
        start, end = pair.split('-')
        for i in range(len(start), len(end)+1):
            if i%2 != 0:
                continue

            if i == len(start):
                startn = start
            else:
                startn = str(10**(i-1))

            if i == len(end):
                endn = end
            else:
                endn = '9'*i

            for x in range(int(startn[:i//2]), int(endn[:i//2])+1):
                candidate = int(str(x)+str(x))
                if int(start) <= candidate <= int(end):
                    invalids.append(candidate)

    print(sum(invalids))

if __name__ == '__main__':
    run()