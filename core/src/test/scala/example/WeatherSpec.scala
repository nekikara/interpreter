package example

import example.core._
import org.scalatest._

class WeatherSpec extends FunSuite with DiagrammedAssertions {
  test("can eval a num expression") {
    assert(Weather.eval(Num(2)) == Num(2))
  }
  test("can eval a plus operation") {
    val expression = Exp(Func(Ope.Plus), Num(1), Num(2))
    val result = Weather.eval(expression)
    assert(result == Num(3))
  }
  test("can eval a minus operation") {
    val expression = Exp(Func(Ope.Minus), Num(10), Num(2))
    val result = Weather.eval(expression)
    assert(result == Num(8))
  }
  test("can eval a multiply operation") {
    val expression = Exp(Func(Ope.Multiply), Num(10), Num(2))
    val result = Weather.eval(expression)
    assert(result == Num(20))
  }
  test("can eval a nested Expression") {
    val expression = Exp(Func(Ope.Plus),
      Exp(Func(Ope.Minus), Num(10), Num(2)),
      Exp(Func(Ope.Multiply),
        Num(2),
        Exp(Func(Ope.Plus), Num(1), Num(2))))
    val result = Weather.eval(expression)
    val expect = (10 - 2) + (2 * (1 + 2))
    assert(result == Num(expect))
  }
  test("can eval a let expression as a lambda") {
    val binds = Binds(Bind(Sym('x), Num(3)), Bind(Sym('y), Num(2)))
    val body = Exp(Func(Ope.Plus), Sym('x), Sym('y))
    val act = Weather.convertLetToLambda(binds, body)
    val expect = (Lambda(Vars(Sym('x), Sym('y)), body), List(Num(3), Num(2)))
    assert(act == expect)
  }
  test("can eval an Expression with let") {
    val exp = Let(Binds( Bind(Sym('x), Num(3)),
                         Bind(Sym('y), Num(2))),
                    Exp(Func(Ope.Plus), Sym('x), Sym('y), Sym('y)))
    val act = Weather.eval(exp)
    val expect = 3 + 2 + 2
    assert(act == Num(expect))
  }
  test("can extend the env stack") {
    val env = EnvStack(List.empty[Env])
    val act = Weather.extendEnvStack(env, Vars(Sym('x), Sym('y)), Seq(Num(2), Num(3)))
    val newEnv = Env(Map(Sym('x) -> Num(2), Sym('y) -> Num(3)))
    val expect = EnvStack(List(newEnv))
    assert(act == expect)
  }
  test("can lookup a var from the env stack") {
    val env = EnvStack(List(Env(Map(Sym('x) -> Num(2), Sym('y) -> Num(3)))))
    val act = Weather.lookupVars(Sym('x), env)
    val expect = Num(2)
    assert(act == expect)
  }
  test("can eval a lambda Expression") {
    val lambda = Lambda( Vars( Sym('x), Sym('y) ),
                  Exp(Func(Ope.Plus), Sym('x), Sym('y), Sym('y)))
    val exp = Exp(lambda, Num(9), Num(20))
    val act = Weather.eval(exp)
    val expect = 9 + 20 + 20
    assert(act == Num(expect))
  }
  test("can eval second lambda Expression") {
    val let = Let(Binds( Bind(Sym('x), Num(2)) ),
                Let(Binds(Bind(Sym('fun), Lambda(Vars(), Sym('x)))),
                  Let(Binds(Bind(Sym('x), Num(1))),
                    Exp(Sym('fun))) ) )
    val act = Weather.eval(let)
    val expect = Num(2)
    assert(act == expect)
  }
  test("can eval third lambda Expression") {
    val let = Let(Binds(Bind(Sym('fun), Lambda(Vars(), Num(1)))),
                    Exp(Sym('fun)))
    val act = Weather.eval(let)
    val expect = Num(1)
    assert(act == expect)
  }
}
