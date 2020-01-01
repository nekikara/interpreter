package example

import example.dsl._

object Main extends App {
  /* Example1 - Calculating the number of elements of a List
   * (define (length li)
   *   (if (isNull li)
   *     0
   *     (+ (length (cdr li)) 1)))
   * (length (list -1 1 2 3)) ==> 4
   */
  val program = Stmt(DefnProgram(
    Bound(List('length, 'li),
      IfE(Apply(Sy('isNull), Ref('li)),
        Ori(N(0)),
        Apply(Sy('+), Apply(Sy('length), Apply(Sy('cdr), Ref('li))), Ori(N(1))))),
    Expr(Apply(Sy('length), Apply(Sy('list), Ori(N(-1)), Ori(N(1)), Ori(N(2)), Ori(N(3))) ))
  ))
  val result = program.runDefault()
  println(s"result: ${result.getOrigin}")
}