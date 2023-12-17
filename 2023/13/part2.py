# If 0, sequence is perfectly mirrored about i.
# If >0, sequence is perfectly mirrored about i
# after fixing some number of smudges.
def mirror_imperfections(seq, i):
    j = 0
    smudges = 0
    while True:
        left = i-j
        right = i+j+1
        if left < 0 or right >= len(seq):
            break
        for chL, chR in zip(seq[left], seq[right]):
            if chL != chR:
                smudges += 1
        j += 1
    return smudges


def transpose(grid):
    return [''.join(col) for col in zip(*grid)]


def reflection_points(seq):
    for i in range(len(seq)-1):
        if mirror_imperfections(seq, i) == 1:
            yield i


def run():
    with open('input.txt') as f:
        blocks = f.read().split('\n\n')
    s = 0
    for b in blocks:
        grid = [line.strip() for line in b.split('\n') if line.strip() != '']
        for i in reflection_points(grid):
            s += (i+1)*100
        for j in reflection_points(transpose(grid)):
            s += j+1
    print(s)


if __name__ == '__main__':
    run()