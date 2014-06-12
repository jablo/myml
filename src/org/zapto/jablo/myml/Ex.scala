/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.{Env, err, typerr}
import scala.collection._
import scala.annotation.tailrec
import Ex.interp

trait ExHelper {
  final implicit def booleanToConst(b: Boolean): Const = if (b) True else False
  final implicit def bigintToConst(b: BigInt): Const = Z(b)
  final implicit def stringToConst(s: String): Const = Str(s)
  final implicit def constToEvalStep(c: Const): EvalStep = Done(c)
}

class MyMLException(msg: String = null, cause: Throwable = null) extends java.lang.Exception(msg, cause)
class TypeErrorException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause)
class UndefinedOperationException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause)

/**
 * Expressions
 * <ul><li>eval method to evaluate the program
 * <li>infix method like toString, but tries to generate an infix notation of the program that could be re-parsed
 * </ul>
 */
abstract class Ex extends ExHelper {
  final def eval(e: Env): Const = interp(this, e)
  def step(e: Env): EvalStep
  val bytecode: List[ByteCode]
  def infix: String
}

// A single step intepretation result - a constant or a new expression and environment to evaluate
abstract sealed class EvalStep
case class Done(r: Const) extends EvalStep
case class Next(e: Ex, env: Env) extends EvalStep

object Ex {
  // We mix mutable and immutable maps, so we have to be explicit about using the generic Map interface for environments
  type Env = scala.collection.Map[String, Const]
  type MutEnv = scala.collection.mutable.Map[String, Const]

  final def typerr(s: String, e: Ex): Nothing = throw new TypeErrorException(s + " in: " + e.infix)
  final def err(s: String, e: Ex): Nothing = throw new MyMLException(s + " " + e.infix)
  final def err(s: String): Nothing = throw new MyMLException(s)

  // Direct interpretation by walking the abstract syntax tree
  @tailrec
  final def interp(e: Ex, env: Env): Const = {
    e step env match {
      case Done(r)        => r
      case Next(e1, env1) => interp(e1, env1)
    }
  }
}

case class ErrorEx(n: String) extends Ex {
  override def step(e: Env): EvalStep = throw new MyMLException("Error: " + n)
  override val bytecode = List(Push(Str(n)), ErrIns)
  override def infix = "error(" + n + ")"
}

case class Var(n: String) extends Ex {
  override def step(e: Env): EvalStep = e get n match {
    case None    => err("Unknown variable", this)
    case Some(v) => Next(v, e)
  }
  override val bytecode = List(Push(n), Lookup)
  override def infix = n
}

case class Par(e: Ex) extends Ex {
  override def step(en: Env): EvalStep = e step en
  override val bytecode = e.bytecode
  override def infix = "(" + e.infix + ")"
}

// Unary operator
abstract class Un(e1: Ex, op: UnOp) extends Ex {
  override def step(e: Env): EvalStep = op.eval(interp(e1, e))
  override val bytecode = e1.bytecode :+ op
  override def infix = op.infix + e1.infix
}
// Binary operator
abstract class Bin(e1: Ex, e2: Ex, op: Op) extends Ex {
  override def step(e: Env): EvalStep = op eval (interp(e1, e), interp(e2, e))
  override val bytecode = e1.bytecode ++ e2.bytecode :+ op
  override def infix = e1.infix + op.infix + e2.infix
}
// Ternary operator
abstract class Tri(e1: Ex, e2: Ex, e3: Ex, op: Op3) extends Ex {
  override def step(e: Env): EvalStep = op eval (interp(e1, e), interp(e2, e), interp(e2, e))
  override val bytecode = e1.bytecode ++ e2.bytecode ++ e3.bytecode :+ op
  override def infix = e1.infix + op.infix + e2.infix
}

case class Neg(e1: Ex) extends Un(e1, ONeg)
case class Add(e1: Ex, e2: Ex) extends Bin(e1, e2, OAdd)
case class Sub(e1: Ex, e2: Ex) extends Bin(e1, e2, OSub)
case class Mul(e1: Ex, e2: Ex) extends Bin(e1, e2, OMul)
case class Div(e1: Ex, e2: Ex) extends Bin(e1, e2, ODiv)
case class Pot(e1: Ex, e2: Ex) extends Bin(e1, e2, OPot)

case class Equ(e1: Ex, e2: Ex) extends Bin(e1, e2, OEq)
case class Neq(e1: Ex, e2: Ex) extends Bin(e1, e2, ONe)
case class Lt(e1: Ex, e2: Ex) extends Bin(e1, e2, OLt)
case class Lte(e1: Ex, e2: Ex) extends Bin(e1, e2, OLe)
case class Gt(e1: Ex, e2: Ex) extends Bin(e1, e2, OGt)
case class Gte(e1: Ex, e2: Ex) extends Bin(e1, e2, OGe)

case class Not(e1: Ex) extends Un(e1, ONot)
case class And(e1: Ex, e2: Ex) extends Bin(e1, e2, OAnd)
case class Or(e1: Ex, e2: Ex) extends Bin(e1, e2, OOr)

case class Cons(e1: Ex, e2: Ex) extends Bin(e1, e2, OCons)
case class Car(e1: Ex) extends Un(e1, OCar)
case class Cdr(e1: Ex) extends Un(e1, OCdr)

case class SubStr(e1: Ex, e2: Ex, e3: Ex) extends Tri(e1, e2, e3, OSubStr) {
  override val bytecode = e1.bytecode ++ e2.bytecode ++ e3.bytecode :+ OSubStr
  override def infix = "sub(" + e1 + ", " + e2 + ", " + e3 + ")"
}
case class TrimStr(e1: Ex) extends Un(e1, OTrimStr) {
  override val bytecode = e1.bytecode :+ OTrimStr
  override def infix = "trim(" + e1 + ")"
}
case class StrLen(e1: Ex) extends Un(e1, OStrLen) {
  override val bytecode = e1.bytecode :+ OStrLen
  override def infix = "strlen(" + e1 + ")"
}
case class ToStr(e1: Ex) extends Un(e1, OToStr) {
  override val bytecode = e1.bytecode :+ OToStr
  override def infix = "tostr(" + e1 + ")"
}

case class Ife(e1: Ex, e2: Ex, e3: Ex) extends Ex {
  override def step(e: Env) = {
    val test = interp(e1, e)
    test match {
      case True  => Next(e2, e)
      case False => Next(e3, e)
      case _     => typerr("boolean", e1)
    }
  }
  override val bytecode = List(Push(Code(e2.bytecode)), Push(Code(e3.bytecode))) ++ e1.bytecode :+ Cond
  def infix = "if " + e1.infix + " then " + e2.infix + " else " + e3.infix
}

case class Fun(fargs: List[String], body: Ex) extends Ex {
  def step(e: Env) = Clo(fargs, body, e)
  override val bytecode = List(Push(Subr(fargs, body.bytecode, NilScope())), MakeClosure)
  def infix = "fun " + fargs.mkString("(", ", ", ")") + " => " + body.infix
}

case class App(fexp: Ex, args: List[Ex]) extends Ex {
  override def step(env: Env) = {
    val fun = interp(fexp, env)
    fun match {
      case _@ Clo(fargs, body, fenv) =>
        val actarg = args map ((arg: Ex) => interp(arg, env))
        val env2 = fenv ++ Map(fargs zip actarg: _*)
        Next(body, env2)
      case _ => typerr("Not a function", fexp)
    }
  }
  override val bytecode = {
    val argscode: List[ByteCode] = (args.map((ex) => ex.bytecode)).flatten
    val fcode = fexp.bytecode
    argscode ++ fcode :+ Call
  }
  def infix = (fexp match {
    case Var(_) => fexp.infix
    case _      => "(" + fexp.infix + ")"
  }) + (args map (_ infix)).mkString("(", ", ", ")");
}

case class LetR(fargs: List[String], args: List[Ex], body: Ex) extends Ex {
  override def step(env: Env) = {
    // Use a mutable map initialized with current env so we can evaluate arguments in their own environmnent, creating a cyclic environment
    val letrecenv = mutable.Map[String, Const](env toList: _*)
    val actarg = args map (interp(_, letrecenv));
    letrecenv ++= fargs zip actarg
    Next(body, letrecenv)
  }
  val bytecode = {
    val argscode: List[ByteCode] = (args.map((ex) => ex.bytecode)).flatten
    val asgncode = fargs.reverse.map((n) => List(Push(n), Assign)).flatten
    RecEnv :: argscode ++ asgncode ++ body.bytecode
  }
  def infix = "let* " + ((fargs zip args) map (p => { val (a, e) = p; a + "=" + e.infix })).mkString("; ") + " in " + body.infix
}

// For the REPL 
case class Def(n: String, e: Ex) extends Ex {
  def step(env: Env): EvalStep = ReplDef(n, interp(e, env))
  val bytecode = e.bytecode ++ List(Push(n), Assign)
  def infix = "def " + n + "=" + e.infix
}

case class Undef(n: String) extends Ex {
  def step(env: Env): EvalStep = ReplUnDef(n)
  val bytecode = List(Push(ReplUnDef(n)))
  def infix = "undef " + n;
}

case class Load(n: String) extends Ex {
  def step(env: Env): EvalStep = ReplLoad(n)
  val bytecode = List(Push(ReplLoad(n)))
  def infix = "load " + n;
}

case class ReLoad() extends Ex {
  def step(env: Env): EvalStep = ReplReLoad()
  val bytecode = List(Push(ReplReLoad()))
  def infix = "reload"
}

case class Compile(e: Ex) extends Ex {
  def step(env: Env): EvalStep = Code(e.bytecode)
  val bytecode = e.bytecode
  def infix = "compile " + e.infix;
}

case class Run(e: Ex) extends Ex {
  def step(env: Env): EvalStep = ReplRun(e.bytecode)
  val bytecode = e.bytecode
  def infix = "compile " + e.infix;
}