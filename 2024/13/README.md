# Day 13

I thought if this ione as: Button A is some vector `U`, Button B is some vector `V`, prize is vector `P`.

Solve for `a`, `b` in `P = a*U + b*V`. Another way to look at that is

```
|Px|   |Ux Vx|   |a|
|Py| = |Uy Vy| * |b|
```

Multiplying both sides by the inverse matrix gives the amount of steps in the `a` and `b` direction, then the number of tokens is `3|a| + |b|`. The machine is only winnable if `a` and `b` are whole numbers.