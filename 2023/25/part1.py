from collections import defaultdict, deque


def visualize():
    with open('input.txt') as f:
        out = [
            'graph G {',
            '  node [shape=point]'
        ]
        for line in f:
            line = line.strip()
            if line == '':
                continue
            a, bs = line.split(':')
            bs_csv = ', '.join(bs.strip().split(' '))
            out.append('  {} -- {};'.format(a, bs_csv))
        out.append('}')
    with open('out.dot.txt', 'w') as f:
        for line in out:
            f.write(line + '\n')


def component_sizes(cut_set):
    adj = defaultdict(list)
    with open('input.txt') as f:
        for line in f:
            line = line.strip()
            if line == '':
                continue
            a, bs = line.split(':')
            bs = bs.strip().split(' ')
            for b in bs:
                if (a, b) not in cut_set:
                    adj[a].append(b)
                    adj[b].append(a)
    sizes = []
    unvisited = set(adj.keys())
    while len(unvisited) > 0:
        q = deque()
        q.append(unvisited.pop())
        size = 1
        while len(q) > 0:
            u = q.popleft()
            for v in adj[u]:
                if v in unvisited:
                    unvisited.remove(v)
                    q.append(v) 
                    size += 1
        sizes.append(size)
    return sizes


if __name__ == '__main__':
    visualize()
    # manually opened the output file in graphviz to see what edges to cut.
    cut_set = [
        ('kzh', 'rks'),
        ('ddc', 'gqm'),
        ('dgt', 'tnz'),
    ]
    cut_set = cut_set + [(b, a) for (a, b) in cut_set]
    sizes = component_sizes(cut_set)
    assert len(sizes) == 2
    print(sizes[0] * sizes[1])