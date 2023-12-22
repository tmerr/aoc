import operator
from pprint import pprint


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


def parse_part(line):
    res = {}
    p = Parser(line)
    p.expect('{')
    while True:
        cat = p.expect_alpha()
        if not cat in ['x', 'm', 'a', 's']:
            raise ParseException('surprising category {}'.format(cat))
        p.expect('=')
        num = p.expect_num()
        res[cat] = num
        if p.accept(','):
            continue
        else:
            p.expect('}')
            break
    return res


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
    parts = []
    with open(fname) as f:
        it = iter(f)
        for line in it:
            line = line.strip()
            if line == '':
                break
            wf = parse_workflow(line)
            workflows[wf.name] = wf
        for line in it:
            line = line.strip()
            if line == '':
                break
            parts.append(parse_part(line))
    return Acceptor(workflows), parts


def run(fname):
    acceptor, parts = parse(fname)
    s = 0
    for part in parts:
        if acceptor(part):
            s += sum(part.values())
    print(s)


if __name__ == '__main__':
    run('input.txt')