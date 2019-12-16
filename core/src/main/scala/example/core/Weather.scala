package example.core

object Opts {
  def plus: Sym = Sym('+)
  def minus: Sym = Sym('-)
  def multiply: Sym = Sym('*)
  def let: Sym = Sym('let)
  def lambda: Sym = Sym('lambda)
  def closure: Sym = Sym('closure)
}

sealed abstract class Expression {
  def ++(es: Expression*): Exp = {
    if (es.isEmpty) {
      Exp(this, Num(0))
    } else {
      es.tail.foldLeft(Exp(this, es.head)) {(acc, exp) => {
        Exp(acc.es :+ exp:_*)
      }}
    }
  }
}
case class Exp(es: Expression*) extends Expression
case class Sym(s: Symbol) extends Expression
case class Num(n: Int) extends Expression {
  def +(that: Num) = Num(this.n + that.n)
  def -(that: Num) = Num(this.n - that.n)
  def *(that: Num) = Num(this.n * that.n)
}
case class Let(let: Sym, binds: Binds, body: Expression) extends Expression
case class Bind(s: Sym, n: Expression) extends Expression
case class Binds(binds: Bind*) extends Expression
case class Lambda(lambda: Sym, vars: Vars, body: Expression) extends Expression
case class Vars(vars: Sym*) extends Expression
case class Closure(closure: Sym, params: Vars, body: Expression, envStack: EnvStack) extends Expression

// Env
case class Env(map: Map[Sym, Expression])
case class EnvStack(envs: List[Env])

object Weather {
  def eval(exp: Expression, envStack: EnvStack = EnvStack(List.empty[Env])): Num =  exp match {
    case Num(n) => Num(n)
    case Let(_, binds, body) => evalLet(binds, body, envStack)
    case Sym(x) => eval(lookupVars(Sym(x), envStack), envStack)
    case Exp(op, es@_*) if op == Opts.plus => operate(plus, envStack, es:_*)
    case Exp(op, es@_*) if op == Opts.minus => operate(minus, envStack, es:_*)
    case Exp(op, es@_*) if op == Opts.multiply => operate(multiply, envStack, es:_*)
    case Exp(Lambda(_, vars, body), args@_*) => evalClosure(vars, body, envStack, args:_*)
    case Exp(Closure(_, vars, body, localEnv), es@_*) => evalClosure(vars, body, localEnv, es:_*)
    case Exp(Sym(x), es@_*) => {
      val exp = lookupVars(Sym(x), envStack)
      val newExp = exp ++ (es:_*)
      eval(newExp, envStack)
    }
    case _ => throw new RuntimeException("Can't match AST")
  }

  def operate(fun: (Num, Num) => Num, envStack: EnvStack, exps: Expression*): Num = {
    val nums = exps.map {
      case Num(n) => Num(n)
      case x => eval(x, envStack)
    }
    nums.tail.foldLeft(nums.head)(fun)
  }

  def plus(x: Num, y: Num): Num = x + y
  def minus(x: Num, y: Num): Num = x - y
  def multiply(x: Num, y: Num): Num = x * y

  def convertLetToLambda(binds: Binds, body: Expression): (Lambda, List[Expression]) = {
    val (vars, args) = divideParamsAndArgs(binds)
    val lambda = Lambda(Opts.lambda, Vars(vars:_*), body)
    (lambda, args)
  }

  def evalLet(binds: Binds, body: Expression, envStack: EnvStack): Num = {
    val (lambda, args) = convertLetToLambda(binds, body)
    val closure = convertLambdaToClosure(lambda.vars, lambda.body, envStack)
    val newExp = args.tail.foldLeft(Exp(closure, args.head)) { (acc, arg) => {
      Exp(acc.es :+ arg:_*)
    }}
    eval(newExp, envStack)
  }

  def divideParamsAndArgs(binds: Binds): (List[Sym], List[Expression]) = {
    val bs = binds.binds
    bs.tail.foldLeft((List(bs.head.s), List(bs.head.n))) { (acc, b) => (acc._1 :+ b.s, acc._2 :+ b.n) }
  }

  def convertLambdaToClosure(vars: Vars, body: Expression, stack: EnvStack): Closure = {
    Closure(Opts.closure, vars, body, stack)
  }

  def evalClosure(vars: Vars, body: Expression, localEnv: EnvStack, args: Expression*): Num = {
    val newEnvStack = extendEnvStack(localEnv, vars, args)
    eval(body, newEnvStack)
  }

  def extendEnvStack(stack: EnvStack, vars: Vars, exps: Seq[Expression]): EnvStack = {
    val list = vars.vars.zip(exps)
    val envMap = list.foldLeft(Map[Sym, Expression]().empty) { (acc, vr) =>
      val e = vr._2 match {
        case Lambda(_, vars, body) => convertLambdaToClosure(vars, body, stack)
        case _ => vr._2
      }
      acc + (vr._1 -> e)
    }
    EnvStack(Env(envMap) :: stack.envs)
  }
  def lookupVars(symbol: Sym, stack: EnvStack): Expression = {
    val env = stack.envs.find(env => {
      env.map.find(pair =>  pair._1 == symbol) match {
        case Some(_) => true
        case None => false
      }
    })
    env match {
      case Some(x) => x.map(symbol) match {
        case Num(n) => Num(n)
        case x => x
      }
      case None => throw new RuntimeException("Not found the var")
    }
  }
}
