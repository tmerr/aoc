nums = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']

def match(txt, rng):
    for i in rng:
      if '0' <= txt[i] <= '9':
        return txt[i]
      for j, name in enumerate(nums):
        if txt[i:i+len(name)] == name:
          return str(j)

def run():
    s = 0
    with open('input.txt') as f:
        for line in f:
            first = match(line, range(len(line)))
            last = match(line, reversed(range(len(line))))
            s += int(first + last)
    print(s)


if __name__ == '__main__':
    
    run()
