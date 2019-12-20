package example

import example.core._
import org.scalatest._

class WeatherSpec extends FunSuite with DiagrammedAssertions {
  test("can eval a num expression") {
    assert(Weather.eval(Prim(Num(2))) == Num(2))
  }
  test("can eval a plus operation") {
    val expression = Exp(Func(Ope.Plus), Prim(Num(1)), Prim(Num(2)))
    val result = Weather.eval(expression)
    assert(result == Num(3))
  }
  test("can eval a minus operation") {
    val expression = Exp(Func(Ope.Minus), Prim(Num(10)), Prim(Num(2)))
    val result = Weather.eval(expression)
    assert(result == Num(8))
  }
  test("can eval a multiply operation") {
    val expression = Exp(Func(Ope.Multiply), Prim(Num(10)), Prim(Num(2)))
    val result = Weather.eval(expression)
    assert(result == Num(20))
  }
  test("can eval a nested Expression") {
    val expression = Exp(Func(Ope.Plus),
      Exp(Func(Ope.Minus), Prim(Num(10)), Prim(Num(2))),
      Exp(Func(Ope.Multiply),
        Prim(Num(2)),
        Exp(Func(Ope.Plus), Prim(Num(1)), Prim(Num(2)))))
    val result = Weather.eval(expression)
    val expect = (10 - 2) + (2 * (1 + 2))
    assert(result == Num(expect))
  }
  test("can eval a let expression as a lambda") {
    val binds = Binds(Bind(Sym('x), Prim(Num(3))), Bind(Sym('y), Prim(Num(2))))
    val body = Exp(Func(Ope.Plus), Sym('x), Sym('y))
    val act = Weather.convertLetToLambda(binds, body)
    val expect = (Lambda(Vars(Sym('x), Sym('y)), body), List(Prim(Num(3)), Prim(Num(2))))
    assert(act == expect)
  }
  test("can eval an Expression with let") {
    val exp = Let(Binds( Bind(Sym('x), Prim(Num(3))),
                         Bind(Sym('y), Prim(Num(2)))),
                    Exp(Func(Ope.Plus), Sym('x), Sym('y), Sym('y)))
    val act = Weather.eval(exp)
    val expect = 3 + 2 + 2
    assert(act == Num(expect))
  }
  test("can extend the env stack") {
    val env = EnvStack(List.empty[Env])
    val act = Weather.extendEnvStack(env, Vars(Sym('x), Sym('y)), Seq(Prim(Num(2)), Prim(Num(3))))
    val newEnv = Env(Map(Sym('x) -> Prim(Num(2)), Sym('y) -> Prim(Num(3))))
    val expect = EnvStack(List(newEnv))
    assert(act == expect)
  }
  test("can lookup a var from the env stack") {
    val env = EnvStack(List(Env(Map(Sym('x) -> Prim(Num(2)), Sym('y) -> Prim(Num(3))))))
    val act = Weather.lookupVars(Sym('x), env)
    val expect = Prim(Num(2))
    assert(act == expect)
  }
  test("can eval a lambda Expression") {
    val lambda = Lambda( Vars( Sym('x), Sym('y) ),
                  Exp(Func(Ope.Plus), Sym('x), Sym('y), Sym('y)))
    val exp = Exp(lambda, Prim(Num(9)), Prim(Num(20)))
    val act = Weather.eval(exp)
    val expect = 9 + 20 + 20
    assert(act == Num(expect))
  }
  test("can eval second lambda Expression") {
    val let = Let(Binds( Bind(Sym('x), Prim(Num(2))) ),
                Let(Binds(Bind(Sym('fun), Lambda(Vars(), Sym('x)))),
                  Let(Binds(Bind(Sym('x), Prim(Num(1)))),
                    Exp(Sym('fun))) ) )
    val act = Weather.eval(let)
    val expect = Num(2)
    assert(act == expect)
  }
  test("can eval third lambda Expression") {
    val let = Let(Binds(Bind(Sym('fun), Lambda(Vars(), Prim(Num(1))))),
                    Exp(Sym('fun)))
    val act = Weather.eval(let)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval a normal If Expression") {
    val ifExp = If(Cond(Cmp.Eq, Prim(Num(2)), Prim(Num(2))),
                  Prim(Num(1)),
                  Prim(Num(2)))
    val act = Weather.eval(ifExp)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval an If expression with a let expression") {
    val ifExp2 = If(Cond(Cmp.Neq, Prim(Num(1)), Prim(Num(2))),
                  Let(Binds(Bind(Sym('x), Prim(Num(1000)))), Exp(Sym('x))),
                  Prim(Num(2)))
    val act2 = Weather.eval(ifExp2)
    val expect2 = Num(1000)
    assert(act2 == expect2)
  }
  test("can eval an If expression with Greater than") {
    val ifExp = If(Cond(Cmp.Gt, Prim(Num(2)), Prim(Num(3))),
                  Prim(Num(1)),
                  Prim(Num(2)))
    val act = Weather.eval(ifExp)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval an If expression with Less than") {
    val ifExp = If(Cond(Cmp.Lt, Prim(Num(2)), Prim(Num(3))),
      Prim(Num(1)),
      Prim(Num(2)))
    val act = Weather.eval(ifExp)
    val expect = Num(2)
    assert(act == expect)
  }
  test("can eval an If expression with Greater than & Eq") {
    val ifExp = If(Cond(Cmp.GtEq, Prim(Num(3)), Prim(Num(3))),
      Prim(Num(1)),
      Prim(Num(2)))
    val act = Weather.eval(ifExp)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval an If expression with Less than & Eq") {
    val ifExp = If(Cond(Cmp.LtEq, Prim(Num(3)), Prim(Num(3))),
      Prim(Num(1)),
      Prim(Num(2)))
    val act = Weather.eval(ifExp)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval a recursive expression but not evaling the recursive expression") {
    val exp = Let(Binds(Bind(Sym('fact), Lambda(Vars(Sym('n)),
                                          If(Cond(Cmp.Gt, Sym('n), Prim(Num(1))),
                                            Prim(Num(1)),
                                            Exp(Sym('fact), Exp(Func(Ope.Minus), Sym('n), Prim(Num(1)))))))),
                Exp(Sym('fact), Prim(Num(0))))
    val act = Weather.eval(exp)
    val expect = Num(1)
    assert(act == expect)
  }
  test("can eval a letrec expression") {
    val exp = LetRec(Binds(Bind(Sym('fact), Lambda(Vars(Sym('n)),
                                              If(Cond(Cmp.Gt, Sym('n), Prim(Num(1))),
                                                Prim(Num(1)),
                                                Exp(Func(Ope.Multiply),
                                                  Sym('n),
                                                  Exp(Sym('fact), Exp(Func(Ope.Minus), Sym('n), Prim(Num(1))))))))),
                Exp(Sym('fact), Prim(Num(4))))
    val act = Weather.eval(exp)
    val expect = Num(24)
    assert(act == expect)
  }
}
