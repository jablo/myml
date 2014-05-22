myml
====

A very small ML-like interpreter to teach me Scala, Scala Parser library,
Packrat parser. And have fun.

Language syntax examples:

1) Simple lambda expressions 

let f = fun (x) => 2*x+4-x^4;
    g = fun (x) => 3*x-4
in
    f(g(5))

2) Recursive lambda expressions
let*
    f = fun (x) => if x = 0 then 1 else x*f(x-1)
in
    f(5)

3) Higher order functions
let 
    d = fun(f) => fun (x) => f(f(x));
    g = fun(x) => 2*x 
in d(g)(5)

4) Rational number arithmetic
let f = fun(x) => 2/3*x^2+1/3*x-2 in f(5)
Gives: Q(49,3)


