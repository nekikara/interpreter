package example.dsl

sealed abstract class EnvBody
case class GlobalFun(f: Seq[Origin] => Origin) extends EnvBody
case class BoolFun(f: Seq[Origin] => Boolean) extends EnvBody
case class ElseFun(f: () => Boolean) extends EnvBody
case class Val(evaluator: Evaluator) extends EnvBody
case class F(f: Fun) extends EnvBody

// Env
case class EnvStacks(envs: List[Map[Symbol, EnvBody]]) {
  def lookup(s: Symbol): Option[EnvBody] = {
    val result = envs.find(env => {
      env.get(s) match {
        case Some(_) => true
        case None => false
      }
    })
    result match {
      case Some(x) => x.get(s)
      case _ => None
    }
  }
  def extend(params: List[Symbol], args: List[EnvBody]): EnvStacks = {
    val env = params.zip(args).foldLeft(Map.empty[Symbol, EnvBody]) {(acc, pair) => {
      acc + (pair._1 -> pair._2)
    }}
    EnvStacks(env :: this.envs)
  }
}

object EnvStacks {
  def default(): EnvStacks = {
    val globalFuncEnv = Map(
      '+ -> GlobalFun((args: Seq[Origin]) => {args.reduce(_ + _)}),
      '- -> GlobalFun((args: Seq[Origin]) => {args.reduce(_ - _)}),
      '* -> GlobalFun((args: Seq[Origin]) => {args.reduce(_ * _)}),
      'nil -> GlobalFun((_: Seq[Origin]) => L()),
      'cons -> GlobalFun((args: Seq[Origin]) => Origins.renewL('cons, args) ),
      'car -> GlobalFun((args: Seq[Origin]) => {
        args.head match {
          case L(ns@_*) => ns.head
          case _ => L()
        }
      }),
      'cdr -> GlobalFun((args: Seq[Origin]) => {
        args.head match {
          case L(ns@_*) => Origins.renewL('cdr, ns.tail)
          case _ => L()
        }
      }),
      'list -> GlobalFun((args: Seq[Origin]) => Origins.renewL('list, args) ),
      '== -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ == _) }),
      '!= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ != _) }),
      '< -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ < _) }),
      '<= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ <= _) }),
      '> -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ > _) }),
      '>= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ >= _) }),
      'isNull -> BoolFun((args: Seq[Origin]) => {
        args.head match {
          case L(ns@_*) => ns.isEmpty
          case _ => false
        }
      }),
      'else -> ElseFun(() => { true }),
    )
    EnvStacks(List(globalFuncEnv))
  }
}

sealed abstract class EvalResult {
  def getOrigin: Option[Origin] = this match {
    case Nope(_) => None
    case Result(v, _) => Some(v)
  }
}
case class Nope(stack: EnvStacks) extends EvalResult
case class Result(value: Origin, stack: EnvStacks) extends EvalResult

trait Interpreter {
  def eval(stack: EnvStacks): EvalResult
}

// Start Program
sealed abstract class Program {
  def runDefault(): EvalResult = this.run(EnvStacks.default())
  def run(stack: EnvStacks): EvalResult
}
case class Stmt(statement: Statement) extends Program {
  override def run(stack: EnvStacks): EvalResult = {
    statement.run(stack)
  }
}
case class Expr(evaluator: Evaluator) extends Program {
  override def run(stack: EnvStacks): EvalResult = {
    evaluator.eval(stack)
  }
}

sealed abstract class Statement {
  def run(stack: EnvStacks): EvalResult
}
case class Defn(definition: Def) extends Statement {
  override def run(stack: EnvStacks): EvalResult = {
    val newStack = definition.load(stack)
    Nope(newStack)
  }
}
case class DefnProgram(definition: Def, program: Program) extends Statement {
  override def run(stack: EnvStacks): EvalResult = {
    val newStack = definition.load(stack)
    program.run(newStack)
  }
}

// Expression that can be evaluated immediately
sealed abstract class Evaluator extends Interpreter
case class Apply(fun: Fun, evaluators: Evaluator*) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    val results = evaluators.map(_.eval(stack) match {
      case Nope(_) => throw new RuntimeException("Include some element that can't solve.")
      case Result(origin: Origin, _) => origin
    })
    val calculated = fun.eval(stack, results:_*)
    Result(calculated, stack)
  }
}
case class LetE(binds: List[(Symbol, Evaluator)], body: Evaluator) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    val env = binds.foldLeft(Map.empty[Symbol, EnvBody]) {(acc, pair) => {
      acc + (pair._1 -> Val(pair._2))
    }}
    val newStack = EnvStacks(env :: stack.envs)
    val lambda = Lda(binds.map(_._1), body)
    val apl = Apply(lambda, binds.map(_._2):_*)
    apl.eval(newStack)
  }
}
case class LetRecE(binds: List[(Symbol, Evaluator)], body: Evaluator) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    val env = binds.foldLeft(Map.empty[Symbol, Evaluator]) {(acc, pair) => {
      acc + (pair._1 -> pair._2)
    }}
    val vars = env.keys.toList
    val letArgs = env.values.toList
    val letArgsEnvBodies = letArgs.map(Val)
    val extEnv = stack.extend(vars, letArgsEnvBodies)
    val argsVals = letArgsEnvBodies.map {
      case Val(evl) => evl.eval(extEnv) match {
        case Result(v, _) => v
        case Nope(_) => throw new RuntimeException(s"Got wrong result: $evl")
      }
      case x => throw new RuntimeException(s"Can't eval except Val: $x")
    }
    val newEnv = extEnv.envs.zipWithIndex.map(pair => {
      if (pair._2 == 0) {
        vars.zip(argsVals).foldLeft(Map.empty[Symbol, EnvBody]) {(acc, p) => {
          acc + (p._1 -> p._2.toEnvBody)
        }}
      } else {
        pair._1
      }
    })
    Apply(Lda(vars, body), letArgs:_*).eval(EnvStacks(newEnv))
  }
}
case class CondE(cnds: (Evaluator, Evaluator)*) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    val f :: rest = cnds.toList
    f._1.eval(stack) match {
      case Result(B(true), _) => f._2.eval(stack)
      case _ => CondE(rest:_*).eval(stack)
    }
  }
}
case class IfE(apl: Apply, tEvl: Evaluator, fEvl: Evaluator) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    apl.eval(stack) match {
      case Result(B(b), _) => if (b) tEvl.eval(stack) else fEvl.eval(stack)
      case _ => throw new RuntimeException("Didn't return Boolean")
    }
  }
}
case class Ori(value: Origin) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = Result(this.value, stack)
}
case class Ref(ref: Symbol) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = {
    stack.lookup(ref) match {
      case Some(GlobalFun(_)) => throw new RuntimeException("Can't evaluate global funs")
      case Some(Val(evl)) => evl.eval(stack)
      case _ => throw new RuntimeException("Not Found a Ref")
    }
  }
}
case class LambdaE(lda: Lda) extends Evaluator {
  override def eval(stack: EnvStacks): EvalResult = Result(FF(lda), stack)
}

sealed abstract class Def {
  def load(stack: EnvStacks): EnvStacks
}
case class Bound(nameArgs: List[Symbol], body: Evaluator) extends Def {
  override def load(stack: EnvStacks): EnvStacks = {
    val gEnv = stack.envs.last
    val name = nameArgs.head
    val params = nameArgs.tail
    val lda = Lda(params, body)
    LambdaE(lda).eval(stack) match {
      case Nope(_) => throw new RuntimeException(s"Cannot load define: $this")
      case Result(v, _) => {
        EnvStacks(List(gEnv + (name -> Val(Ori(v)))))
      }
    }
  }
}
case class Separated(name: Symbol, body: Fun) extends Def {
  override def load(stack: EnvStacks): EnvStacks = {
    stack
  }
}

sealed abstract class Fun {
  def eval(stack: EnvStacks, args: Origin*): Origin
}
case class Cls(params: List[Symbol], body: Evaluator, local: EnvStacks) extends Fun {
  override def eval(stack: EnvStacks, args: Origin*): Origin = {
    val newStack = local.extend(params, args.map(arg => Val(Ori(arg))).toList)
    body.eval(newStack) match {
      case Nope(_) => throw new RuntimeException("Failed to evaluate a Closure")
      case Result(v, _) => v
    }
  }
}
case class Lda(vars: List[Symbol], body: Evaluator) extends Fun {
  override def eval(stack: EnvStacks, args: Origin*): Origin = {
    val cls = Cls(vars, body, stack)
    val newExp = Apply(cls, args.map(Ori):_*)
    newExp.eval(stack) match {
      case Nope(_) => throw new RuntimeException("Failed to evaluate a Lambda")
      case Result(v, _) => v
    }
  }
}
case class Sy(value: Symbol) extends Fun {
  override def eval(stack: EnvStacks, args: Origin*): Origin = {
    stack.lookup(value) match {
      case None => throw new RuntimeException("Not Found a function")
      case Some(GlobalFun(f)) => {
        f(args)
      }
      case Some(BoolFun(f)) => B(f(args))
      case Some(ElseFun(f)) => B(f())
      case Some(Val(evl)) => evl.eval(stack) match {
        case Nope(_) => throw new RuntimeException("Failed to evaluate Symbol evaluation")
        case Result(v, _) => v match {
          case FF(f) => {
            f.eval(stack, args:_*)
          }
          case x => x
        }
      }
      case Some(F(fun)) => fun.eval(stack, args:_*)
    }
  }
}

sealed trait Origin {
  def +(that: Origin): Origin = {
    (this, that) match {
      case (a: N, b: N) => N(a.value + b.value)
      case (a: N, b: L) => L(a +: b.values:_*)
      case (a: L, b: N) => L(a.values :+ b:_*)
      case (a: L, b: L) => L(a.values ++ b.values:_*)
      case _ => throw new RuntimeException(s"Can't solve $this + $that")
    }
  }
  def -(that: Origin): Origin = {
    (this, that) match {
      case (a: N, b: N) => N(a.value - b.value)
      case (a: N, b: L) => L(b.values.map(x => N(a.value - x.value)):_*)
      case (a: L, b: N) => L(a.values.map(x => N(x.value - b.value)):_*)
      case (a: L, b: L) => L(a.values.zip(b.values).map(p => N(p._1.value - p._2.value)):_*)
      case _ => throw new RuntimeException(s"Can't solve $this - $that")
    }
  }
  def *(that: Origin): Origin = {
    (this, that) match {
      case (a: N, b: N) => N(a.value * b.value)
      case (a: N, b: L) => L(b.values.map(x => N(x.value * a.value)):_*)
      case (a: L, b: N) => L(a.values.map(x => N(x.value * b.value)):_*)
      case (a: L, b: L) => L(a.values.zip(b.values).map(p => N(p._1.value * p._2.value)):_*)
      case _ => throw new RuntimeException(s"Can't solve $this * $that")
    }
  }
  def <(that: Origin): Boolean = {
    (this, that) match {
      case (a: N, b: N) => a.value < b.value
      case _ => throw new RuntimeException(s"Can't solve: $this < $that")
    }
  }
  def <=(that: Origin): Boolean = {
    (this, that) match {
      case (a: N, b: N) => a.value <= b.value
      case _ => throw new RuntimeException(s"Can't solve: $this <= $that")
    }
  }
  def >(that: Origin): Boolean = {
    (this, that) match {
      case (a: N, b: N) => a.value > b.value
      case _ => throw new RuntimeException(s"Can't solve: $this > $that")
    }
  }
  def >=(that: Origin): Boolean = {
    (this, that) match {
      case (a: N, b: N) => a.value >= b.value
      case _ => throw new RuntimeException(s"Can't solve: $this >= $that")
    }
  }
  def toEnvBody: EnvBody = Val(Ori(this))
}
case class N(value: Int) extends Origin
case class L(values: N*) extends Origin
case class B(value: Boolean) extends Origin
case class FF(value: Fun) extends Origin

object Origins {
  def all(args: Seq[Origin])(block: (Origin, Origin) => Boolean): Boolean = {
    if (args.size < 2) {
      throw new RuntimeException(s"Less number of args: $args")
    }
    if (args.size < 3) {
      block(args.head, args.last)
    } else {
      val f :: s :: rest = args.toList
      rest.foldLeft((s, f == s)) { (acc, arg) =>
        (arg, block(acc._1, arg) && acc._2)
      }._2
    }
  }

  def renewL(name: Symbol, args: Seq[Origin]): Origin = {
    args.foldLeft(L()) { (acc: L, arg) => {
      arg match {
        case N(n) => L(acc.values.toList :+ N(n):_*)
        case L(l@_*) => {
          val nList = acc.values.toList ++ l.toList
          L(nList:_*)
        }
        case _ => throw new RuntimeException("Failed to cons")
      }
    }}
  }
}

