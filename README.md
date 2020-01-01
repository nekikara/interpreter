# Scheme like DSL

This project is to learn Scala, writing a DSL that does arithmetic calculus in a Scheme-like AST.

## Implemented
- Define
- Recursion (Letrec)
- Let
- If
- Cond
- Lambda
- Closure

## Example
##### Scheme
```scheme
(define (length li)
 (if (isNull li)
   0
   (+ (length (cdr li)) 1)))
(length (list -1 1 2 3)) ;; ==> 4
```
##### Scala
```scala
val program = Stmt(DefnProgram(
Bound(List('length, 'li),
  IfE(Apply(Sy('isNull), Ref('li)),
    Ori(N(0)),
    Apply(Sy('+), Apply(Sy('length), Apply(Sy('cdr), Ref('li))), Ori(N(1))))),
Expr(Apply(Sy('length), Apply(Sy('list), Ori(N(-1)), Ori(N(1)), Ori(N(2)), Ori(N(3))) ))
))
val result = program.runDefault()
```