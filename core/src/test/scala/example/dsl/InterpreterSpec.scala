package example.dsl

import org.scalatest._

class InterpreterSpec extends FunSuite with DiagrammedAssertions {
  val stack: EnvStacks = EnvStacks.default()
  test("can eval Ori evaluators") {
    val n = N(1)
    val originN = Ori(n)
    val actual = originN.eval(stack)
    val expect = Result(n, stack)
    assert(actual == expect)

    val l = L(N(1), N(2))
    val originL = Ori(l)
    val actual2 = originL.eval(stack)
    val expect2 = Result(l, stack)
    assert(actual2 == expect2)
  }

  test("can eval Apply evaluators") {
    // 1 N
    val apply1 = Apply(Sy('+), Ori(N(1)), Ori(N(2)))
    val actual1 = apply1.eval(stack)
    assert(actual1 == Result(N(3), stack))

    // 2 L
    val apply2 = Apply(Sy('+), Ori(L(N(1), N(2))), Ori(L(N(4), N(3))))
    val actual2 = apply2.eval(stack)
    assert(actual2 == Result(L(N(1), N(2), N(4), N(3)), stack))
  }

  test("can eval Let evaluators") {
    val let1 = LetE(List(('x, Ori(N(3))), ('y, Ori(N(2))) ),
                    Apply(Sy('+), Ref('x), Ref('y)))
    val actual1 = let1.eval(stack)
    assert(actual1.getOrigin.contains(N(5)))
  }

  test("can eval LetRec evaluations") {
    val letRec1 = LetRecE(List(('fact,
      LambdaE(Lda(List('n),
        IfE(Apply(Sy('<), Ref('n), Ori(N(1))),
          Ori(N(1)),
          Apply(Sy('*), Ref('n), Apply(Sy('fact), Apply(Sy('-), Ref('n), Ori(N(1)))) )))))),
      Apply(Sy('fact), Ori(N(3))))
    val actual1 = letRec1.eval(stack)
    assert(actual1.getOrigin.contains(N(6)))
  }

  test("can eval Lambda evaluators") {
    val lda1 = Lda(List('x, 'y), Apply(Sy('*), Ref('y), Ori(N(100))))
    val apply1 = Apply(lda1, Ori(N(2)), Ori(N(20)))
    val actual1 = apply1.eval(stack)
    assert(actual1.getOrigin.contains(N(20 * 100)))
  }

  test("can eval If evaluators") {
    val if1 = IfE(Apply(Sy('==), Ori(N(1)), Ori(N(2))), Ori(B(true)), Ori(B(false)))
    val actual1 = if1.eval(stack)
    assert(actual1.getOrigin.contains(B(false)))

    val if2 = IfE(Apply(Sy('<=), Ori(N(2)), Ori(N(2)), Ori(N(100))), Ori(B(true)), Ori(B(false)))
    val actual2 = if2.eval(stack)
    assert(actual2.getOrigin.contains(B(true)))
  }

  test("can eval Cond evaluators") {
    val cond1 = CondE(
      (Apply(Sy('>), Ori(N(1)), Ori(N(2))), Ori(N(1))),
      (Apply(Sy('>=), Ori(N(4)), Ori(N(10))), Ori(N(2))),
      (Apply(Sy('<=), Ori(N(1000)), Ori(N(10))), Ori(N(3))),
      (Apply(Sy('else)), Ori(N(-1)))
    )
    val actual1 = cond1.eval(stack)
    assert(actual1.getOrigin.contains(N(-1)))
  }

  test("An Origin should calculate another origins") {
    val actual1 = N(1) + N(2)
    assert(actual1 == N(3))
    val actual2 = L(N(1), N(3)) + L(N(5), N(4))
    assert(actual2 == L(N(1), N(3), N(5), N(4)))
    val actual3 = L(N(1), N(3)) - N(1)
    assert(actual3 == L(N(0), N(2)))
    val actual4 = L(N(1), N(3)) * L(N(10), N(20))
    assert(actual4 == L(N(10), N(60)))
  }
}
