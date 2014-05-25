/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.Env
import scala.collection._

/**
 * Expressions
 * <ul><li>eval method to evaluate the program
 * <li>infix method like toString, but tries to generate an infix notation of the program that could be re-parsed
 * </ul>
 */
abstract class Ex {
  def eval(e: Env): Const
  def infix: String
  protected def typerr(s: String, e: Ex): Nothing = throw new TypeErrorException("Expected " + s + " expression: " + e.infix)
}

object Ex {
  // We mix mutable and immutable maps, so we have to be explicit about using the generic Map interface for environments
  type Env = scala.collection.Map[String, Const]
}

class MyMLException(msg: String = null, cause: Throwable = null) extends java.lang.Exception(msg, cause) {}
class TypeErrorException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause) {}
class UndefinedOperationException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause) {}

case class Var(n: String) extends Ex {
  override def eval(e: Env): Const = e(n) // 
  override def infix = n
}

case class Par(e: Ex) extends Ex {
  override def eval(en: Env): Const = e eval en
  override def infix = "(" + e.infix + ")"
}

// Unary operator
abstract class Un(e1: Ex, op: UnOp) extends Ex {
  override def infix = op.infix + e1.infix
  override def eval(e: Env): Const = op.eval((e1 eval e))
}

// Binary operator
abstract class Bin(e1: Ex, e2: Ex, op: Op) extends Ex {
  override def infix = e1.infix + op.infix + e2.infix
  override def eval(e: Env): Const = op eval (e1 eval e, e2 eval e)
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

case class Ife(e1: Ex, e2: Ex, e3: Ex) extends Ex {
  def eval(e: Env) = {
    val test = e1 eval e
    test match {
      case True  => e2 eval e
      case False => e3 eval e
      case _     => typerr("boolean", e1)
    }
  }
  def infix = "if " + e1.infix + " then " + e2.infix + " else " + e3.infix
}

case class Fun(fargs: List[String], body: Ex) extends Ex {
  def eval(e: Env) = Clo(fargs, body, e)
  def infix = "fun " + fargs.mkString("(", ", ", ")") + " => " + body.infix
}

case class Clo(fargs: List[String], body: Ex, env: Env) extends Const {
  // avoid printing environment values - letrec creates cyclic environment
  override def infix = Fun(fargs, body).infix + "@" + (env keys).mkString("{", ",", "}")
  override def toString = "Clo(" + fargs + ", " + body + ", " + (env keys).mkString("{", ",", "}") + ")"
}

case class App(fexp: Ex, args: List[Ex]) extends Ex {
  def eval(env: Env) = {
    val _@ Clo(fargs, body, fenv) = fexp eval env
    val actarg = args map (_ eval env)
    val env2 = fenv ++ Map(fargs zip actarg: _*)
    body eval env2
  }
  def infix = (fexp match {
    case Var(_) => fexp.infix
    case _      => "(" + fexp.infix + ")"
  }) + (args map (_ infix)).mkString("(", ", ", ")");
}

case class LetR(fargs: List[String], args: List[Ex], body: Ex) extends Ex {
  def eval(env: Env) = {
    // Use a mutable map initialized with current env so we can evaluate arguments in their own environmnent, creating a cyclic environment
    val letrecenv = mutable.Map[String, Const](env toList: _*)
    val actarg = args map (_ eval letrecenv);
    letrecenv ++= fargs zip actarg
    body eval letrecenv
  }
  def infix = "let* " + ((fargs zip args) map (p => { val (a, e) = p; a + "=" + e.infix })).mkString ("; ") + " in " + body.infix
}

// For the REPL we use a mutable Map so we can dynamically add and remove function defs
case class Def(n: String, e: Ex) extends Ex {
  def eval(env: Env): Const = ReplDef(n, e eval env)
  def infix = "def " + n + "=" + e.infix
}

case class Undef(n: String) extends Ex {
  def eval(env: Env): Const = ReplUnDef(n)
  def infix = "undef " + n;
}
