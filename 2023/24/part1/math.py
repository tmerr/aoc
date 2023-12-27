from sympy import Symbol, solve, Eq, simplify

def run():
    s1x = Symbol('a.S[0]')
    s1y = Symbol('a.S[1]')
    v1x = Symbol('a.V[0]')
    v1y = Symbol('a.V[1]')
    t1 = Symbol('t1')
    s2x = Symbol('b.S[0]')
    s2y = Symbol('b.S[1]')
    v2x = Symbol('b.V[0]')
    v2y = Symbol('b.V[1]')
    t2 = Symbol('t2')
    soln = solve([
        Eq(s1x + v1x*t1, s2x + v2x*t2),
        Eq(s1y + v1y*t1, s2y + v2y*t2),
    ], (t1, t2))
    print(soln[t1])
    print(soln[t2])
    print(simplify(s1x + v1x * t1))
    print(simplify(s1y + v1y * t1))


if __name__ == '__main__':
    run()