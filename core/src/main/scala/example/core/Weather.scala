package example.core

sealed abstract class Primitive
case class Num(n: Int) extends Primitive {
  override def toString: String = s"$n"
  def +(that: Num): Num = Num(this.n + that.n)
  def -(that: Num): Num = Num(this.n - that.n)
  def *(that: Num): Num = Num(this.n * that.n)
  def <(that: Num): Bool = Bool(this.n < that.n)
  def >(that: Num): Bool = Bool(this.n > that.n)
  def <=(that: Num): Bool = Bool(this.n <= that.n)
  def >=(that: Num): Bool = Bool(this.n >= that.n)
}
case class Bool(b: Boolean) extends Primitive

object Ope extends Enumeration {
  protected case class Val(name: String, func: (Num, Num) => Num) extends super.Val {
    override def toString(): String = name
  }
  import scala.language.implicitConversions
  implicit def valueToOpeVal(x: Value): Val = x.asInstanceOf[Val]

  val Plus: Val = Val(":+", (x: Num, y: Num) => x + y)
  val Minus: Val = Val(":-", (x: Num, y: Num) => x - y)
  val Multiply: Val = Val(":*", (x: Num, y: Num) => x * y)
}

object Cmp extends Enumeration {
  protected case class Val(name: String, func: (Num, Num) => Bool) extends super.Val {
    override def toString(): String = name
  }
  import scala.language.implicitConversions
  implicit def valueToCmpVal(x: Value): Val = x.asInstanceOf[Val]

  val Eq: Val = Val(":=", (x: Num, y: Num) => Bool(x == y))
  val Neq: Val = Val(":!=", (x: Num, y: Num) => Bool(x != y))
  val Gt: Val = Val(":<", (x: Num, y: Num) => x < y)
  val Lt: Val = Val(":>", (x: Num, y: Num) => x > y)
  val GtEq: Val = Val(":<=", (x: Num, y: Num) => x <= y)
  val LtEq: Val = Val(":>=", (x: Num, y: Num) => x >= y)
}

sealed abstract class Expression {
  def ++(es: Expression*): Exp = {
    if (es.isEmpty) {
      Exp(this, Prim(Num(0)))
    } else {
      es.tail.foldLeft(Exp(this, es.head)) {(acc, exp) => {
        Exp(acc.es :+ exp:_*)
      }}
    }
  }
}
case class Exp(es: Expression*) extends Expression {
  override def toString: String = s"(${es.map(_.toString).mkString(", ")}/)"
}
case class Sym(s: Symbol) extends Expression {
  override def toString: String = s":${s.name}"
}
case class Prim(p: Primitive) extends Expression {
  override def toString: String = s"$p"
}
case class Func(operator: Ope.Value) extends Expression {
  override def toString: String = s"$operator"
}
case class Let(binds: Binds, body: Expression) extends Expression {
  override def toString: String = s"<:let, $binds,\n\t$body/>"
}
case class LetRec(binds: Binds, body: Expression) extends Expression
case class Cond(cmp: Cmp.Value, left: Expression, right: Expression) extends Expression
case class If(cond: Cond, tExp: Expression, fExp: Expression) extends Expression
case class Bind(s: Sym, n: Expression) extends Expression
case class Binds(binds: Bind*) extends Expression
case class Lambda(vars: Vars, body: Expression) extends Expression {
  override def toString: String = s"<:lambda, $vars,\n\t$body/>"
}
case class Vars(vars: Sym*) extends Expression {
  override def toString: String = s"[${vars.map(_.toString).mkString(", ")}/]"
}
case class Closure(params: Vars, body: Expression, envStack: EnvStack) extends Expression

// Env
case class Env(map: Map[Sym, Expression])
case class EnvStack(envs: List[Env])

object Weather {
  @scala.annotation.tailrec
  def eval(exp: Expression, envStack: EnvStack = EnvStack(List.empty[Env])): Num = {
    downExpression(exp, envStack) match {
      case Prim(Num(n)) => Num(n)
      case x => eval(x, envStack)
    }
  }

  def evalList(envStack: EnvStack, exps: Expression*): Seq[Num] = {
    exps.map {
      case Prim(Num(n)) => Num(n)
      case x => eval(x, envStack)
    }
  }

  def operate(fun: (Num, Num) => Num, envStack: EnvStack, exps: Expression*): Num = {
    val nums = evalList(envStack, exps:_*)
    nums.tail.foldLeft(nums.head)(fun) match {
      case Num(n) => Num(n)
      case x => throw new RuntimeException(s"Can't operate this $x")
    }
  }

  def downExpression(exp: Expression, stack: EnvStack): Expression = exp match {
    case Prim(x) => Prim(x)
    case Let(binds, body) => Prim(evalLet(binds, body, stack))
    case LetRec(binds, body) => Prim(evalLetRec(binds, body, stack))
    case If(cond, tExp: Expression, fExp: Expression) => Prim(evalIf(cond, tExp, fExp, stack))
    case Sym(x) => lookupVars(Sym(x), stack)
    case Exp(ope, es@_*) => Prim(evalOperation(ope, stack, es:_*))
    case Lambda(_, _) => exp
    case _ => throw new RuntimeException(s"Can't find AST: $exp")
  }

  def evalOperation(ope: Expression, stack: EnvStack, es: Expression*): Num = {
    val evaluatedEs = es.map(downExpression(_, stack))
    ope match {
      case Prim(Num(n)) => Num(n)
      case Lambda(vars, body) => evalClosure(vars, body, stack, evaluatedEs:_*)
      case Closure(vars, body, localEnv) => evalClosure(vars, body, localEnv, evaluatedEs:_*)
      case Func(ope: Ope.Value) => operate(ope.func, stack, evaluatedEs:_*)
      case Sym(x) =>
        val exp = lookupVars(Sym(x), stack)
        val newExp = exp ++ (evaluatedEs:_*)
        eval(newExp, stack)
      case _ => throw new RuntimeException(s"Can't match any Operation:\n$ope\n$stack\n$es")
    }
  }

  def evalIf(cond: Cond, t: Expression, f: Expression, envStack: EnvStack): Num = {
    val left = eval(cond.left, envStack)
    val right = eval(cond.right, envStack)
    val b = cond.cmp.func(left, right)
    b match {
      case Bool(true) => eval(t, envStack)
      case _ => eval(f, envStack)
    }
  }

  def convertLetToLambda(binds: Binds, body: Expression): (Lambda, List[Expression]) = {
    val (vars, args) = divideParamsAndArgs(binds)
    val lambda = Lambda(Vars(vars:_*), body)
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

  def evalLetRec(binds: Binds, body: Expression, envStack: EnvStack): Num = {
    val (params, args) = divideParamsAndArgs(binds)
    val tmpMap = params.foldLeft(Map[Sym, Expression]().empty) { (acc, vr) =>
      acc + (vr -> Prim(Num(-1)))
    }
    val extendEnv = extendEnvStack(envStack, Vars(tmpMap.keys.toSeq:_*), tmpMap.values.toSeq)
    val argsVals: List[Expression] = args.map {
      case Prim(Num(n)) => Prim(Num(n))
      case Lambda(vars: Vars, body: Expression) => Lambda(vars, body)
      case x => Prim(eval(x, extendEnv))
    }
    val newEnv = extendEnv.envs.zipWithIndex.map((pair) => {
      if (pair._2 == 0) {
        val env = params.zip(argsVals).foldLeft(Map[Sym, Expression]().empty)((acc, p) => {
          acc + (p._1 -> p._2)
        })
        Env(env)
      } else {
        pair._1
      }
    })
    val newEnvStack = EnvStack(newEnv)
    val newExp = Lambda(Vars(params:_*), body) ++ (args:_*)
    eval(newExp, newEnvStack)
  }

  def divideParamsAndArgs(binds: Binds): (List[Sym], List[Expression]) = {
    val bs = binds.binds
    bs.tail.foldLeft((List(bs.head.s), List(bs.head.n))) { (acc, b) => (acc._1 :+ b.s, acc._2 :+ b.n) }
  }

  def convertLambdaToClosure(vars: Vars, body: Expression, stack: EnvStack): Closure = {
    Closure(vars, body, stack)
  }

  def evalClosure(vars: Vars, body: Expression, localEnv: EnvStack, args: Expression*): Num = {
    val newEnvStack = extendEnvStack(localEnv, vars, args)
    eval(body, newEnvStack)
  }

  def extendEnvStack(stack: EnvStack, vars: Vars, exps: Seq[Expression]): EnvStack = {
    val list = vars.vars.zip(exps)
    val envMap = list.foldLeft(Map[Sym, Expression]().empty) { (acc, vr) =>
      val e = vr._2 match {
        case Lambda(vars, body) => convertLambdaToClosure(vars, body, stack)
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
        case Prim(n) => Prim(n)
        case x => x
      }
      case None => throw new RuntimeException(s"Not found the var: $symbol, $stack")
    }
  }
}
