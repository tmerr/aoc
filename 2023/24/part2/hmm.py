# Search the data for interesting patterns.
from collections import Counter

ctr = Counter()
with open('input.txt') as f:
  for line in f:
    L, R = line.split('@')
    L = list(map(int, L.split(', ')))
    R = list(map(int, R.split(', ')))
    ctr[('S01', L[0], L[1])] += 1
    ctr[('S02', L[0], L[2])] += 1
    ctr[('S12', L[1], L[2])] += 1
    ctr[('V01', R[0], R[1])] += 1
    ctr[('V02', R[0], R[2])] += 1
    ctr[('V12', R[1], R[2])] += 1
    for i, c in enumerate(R):
      ctr[(i, L[i], c)] += 1
print(ctr.most_common(10))