def run():
    grid = []
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            grid.append(list(line))
    # Transpose grid since it's more natural to operate within a row.
    # Now our job is to roll rocks east.
    grid = list(map(list, zip(*grid)))
    total_load = 0
    for line in grid:
        rollto = 0
        for i, ch in enumerate(line):
            match ch:
                case 'O':
                    line[i] = '.'
                    line[rollto] = 'O'
                    # above grid reassignments are only for print debugging.
                    total_load += len(line)-rollto
                    rollto += 1
                case '#':
                    rollto = i+1
                case '.':
                    pass
    print(total_load)


if __name__ == '__main__':
    run()