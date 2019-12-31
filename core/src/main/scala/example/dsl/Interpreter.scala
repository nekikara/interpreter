package example.dsl

import example.core.Expression

sealed abstract class EnvBody
case class GlobalFun(f: Seq[Origin] => Origin) extends EnvBody
case class BoolFun(f: Seq[Origin] => Boolean) extends EnvBody
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
      '== -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ == _) }),
      '!= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ != _) }),
      '< -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ < _) }),
      '<= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ <= _) }),
      '> -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ > _) }),
      '>= -> BoolFun((args: Seq[Origin]) => { Origins.all(args)(_ >= _) }),
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
sealed abstract class Program // extends Interpreter
case class Stmt(statement: Statement) extends Program
case class Expr(expression: Evaluator) extends Program

sealed abstract class Statement
case class Defn(definition: Def) extends Statement
case class DefnProgram(definition: Def, program: Program) extends Statement

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
//case class LetRec() extends Evaluator
//case class Cond() extends Evaluator
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

sealed abstract class Def
case class Bound(nameArgs: List[Symbol], body: Expression) extends Def
case class Separated(name: Symbol, body: Fun) extends Def

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
      case Some(GlobalFun(f)) => f(args)
      case Some(BoolFun(f)) => B(f(args))
      case Some(Val(evl)) => evl.eval(stack) match {
        case Nope(_) => throw new RuntimeException("Failed to evaluate Symbol evaluation")
        case Result(v, _) => v
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
}
case class N(value: Int) extends Origin
case class L(values: N*) extends Origin
case class B(value: Boolean) extends Origin

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
}
