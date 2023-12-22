import operator
from multiprocessing import Pool


class ParseException(Exception):
    def __init__(self, message, data=None, pos=None):
        if data or pos:
            message = 'with data "{}" at position {}: {}'.format(data, pos, message)
        super().__init__(message)


class Parser:
    def __init__(self, data):
        self.i = 0
        self.data = data

    def parse_exception(self, message):
        return ParseException(message, data=self.data, pos=self.i)
    
    def accept(self, ch):
        if self.data[self.i] == ch:
            self.i += 1
            return True
        return False
    
    def expect(self, ch):
        if not self.accept(ch):
            raise self.parse_exception("expected " + ch)
    
    def expect_alpha(self):
        s = self.i
        try:
            while self.data[self.i].isalpha():
                self.i += 1
        except IndexError:
            raise self.parse_exception("unexpected end of input parsing alpha")
        res = self.data[s:self.i]
        if res == '':
            raise self.parse_exception("expected alpha")
        return res
    
    def expect_num(self):
        s = self.i
        try:
            while '0' <= self.data[self.i] <= '9':
                self.i += 1
        except IndexError:
            raise self.parse_exception("unexpected end of input parsing num")
        res = self.data[s:self.i]
        if res == '':
            raise self.parse_exception("expected num")
        return int(res)


class Workflow:
    def __init__(self, name, rules):
        self.name = name
        self.rules = rules

    def __call__(self, catdict):
        for rule in self.rules:
            if (action := rule(catdict)):
                return action
        return None

    def __repr__(self):
        return 'Workflow({}, {})'.format(self.name, repr(self.rules))


class Rule:
    def __init__(self, op, cat, num, action):
        self.op = op
        self.cat = cat
        self.num = num
        self.action = action

    def __call__(self, catdict):
        if self.op(catdict[self.cat], self.num):
            return self.action
        return None

    def __repr__(self):
        op = self.op
        if op == operator.lt:
            op = '<'
        elif op == operator.gt:
            op = '>'
        return 'Rule({} {} {} -> {})'.format(self.cat, op, self.num, self.action)
    

class AlwaysRule:
    def __init__(self, action):
        self.action = action
    
    def __call__(self, catdict):
        return self.action

    def __repr__(self):
        return 'AlwaysRule(-> {})'.format(self.action)


def expect_rule(p):
    first = p.expect_alpha()
    is_cat = first in ['x', 'm', 'a', 's']
    if is_cat and p.accept('<'):
        op = operator.lt
    elif is_cat and p.accept('>'):
        op = operator.gt
    else:
        return AlwaysRule(first)
    num = p.expect_num()
    p.expect(':')
    action = p.expect_alpha()
    return Rule(op, first, num, action)


def parse_workflow(line):
    p = Parser(line)
    label = p.expect_alpha()
    p.expect('{')
    rules = []
    while True:
        rules.append(expect_rule(p))
        if p.accept(','):
            continue
        else:
            p.expect('}')
            break
    return Workflow(label, rules)


class Acceptor:
    def __init__(self, workflows):
        self.workflows = workflows
    
    def __call__(self, catdict):
        cur = 'in'
        while True:
            wf = self.workflows[cur]
            match wf(catdict):
                case 'R':
                    return False
                case 'A':
                    return True
                case action: 
                    cur = action
        assert False


def parse(fname):
    workflows = {}
    with open(fname) as f:
        it = iter(f)
        for line in it:
            line = line.strip()
            if line == '':
                break
            wf = parse_workflow(line)
            workflows[wf.name] = wf
    return workflows


def critical_points(workflows):
    points = {k: [1, 4001] for k in 'xmas'}
    for wf in workflows.values():
        for rule in wf.rules:
            if isinstance(rule, AlwaysRule):
                continue
            ps = points[rule.cat]
            if rule.op == operator.lt:
                ps.append(rule.num)
            elif rule.op == operator.gt:
                ps.append(rule.num+1)
    for ps in points.values():
        ps.sort()
    return points


def slice_chunk(seq, part, numparts):
    partsize = len(seq)//numparts
    if part == numparts - 1:
        return seq[part*partsize:]
    return seq[part*partsize:(part+1)*partsize]


def run(args):
    fname, part, numparts = args
    workflows = parse(fname)
    crit = critical_points(workflows)
    acc = Acceptor(workflows)
    xs = slice_chunk(list(zip(crit['x'], crit['x'][1:])), part, numparts)
    s = 0
    for progress, (x1, x2) in enumerate(xs):
        print('worker {} processing item {} of {}'.format(part, progress, len(xs)))
        for m1, m2 in zip(crit['m'], crit['m'][1:]):
            for a1, a2 in zip(crit['a'], crit['a'][1:]):
                for s1, s2 in zip(crit['s'], crit['s'][1:]):
                    d = {'x': x1, 'm': m1, 'a': a1, 's': s1}
                    if acc(d):
                        s += (x2-x1)*(m2-m1)*(a2-a1)*(s2-s1)
    return s


if __name__ == '__main__':
    pool_size = 6
    with Pool(pool_size) as p:
        args = []
        for i in range(pool_size):
            args.append(('input.txt', i, pool_size))
        print(args)
        print(sum(p.map(run, args)))