/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import scala.language.postfixOps
import scala.collection._
import UnOp._
import Op._
import Op3._

/**
 * Expressions
 * <ul><li>eval method to evaluate the program
 * <li>infix method like toString, but tries to generate an infix notation of the program that could be re-parsed
 * </ul>
 */
abstract class Ex extends ExHelper {
  def infix: String
}

object Ex {
  // We mix mutable and immutable maps, so we have to be explicit about using the generic Map interface for environments
  type Env = scala.collection.Map[String, Const]
  type MutEnv = scala.collection.mutable.Map[String, Const]
}

case class ErrorEx(n: String) extends Ex {
  override def infix = "error(" + n + ")"
}

case class Var(n: String) extends Ex {
  override def infix = n
}

case class Par(e: Ex) extends Ex {
  override def infix = "(" + e.infix + ")"
}

// Unary operator
case class Un(e1: Ex, op: UnOp) extends Ex {
  override def infix = op.infix + e1.infix
}

object Un {
  def Neg(e1: Ex): Ex = Un(e1, ONeg)
  def Not(e1: Ex): Ex = Un(e1, ONot)

  def Car(e1: Ex): Ex = Un(e1, OCar)
  def Cdr(e1: Ex): Ex = Un(e1, OCdr)

  def TrimStr(e1: Ex): Ex = Un(e1, OTrimStr)
  def StrLen(e1: Ex): Ex = Un(e1, OStrLen)
  def ToStr(e1: Ex): Ex = Un(e1, OToStr)
}

// Binary operator
case class Bin(e1: Ex, e2: Ex, op: Op) extends Ex {
  override def infix = e1.infix + op.infix + e2.infix
}

object Bin {
  def Add(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OAdd)
  def Sub(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OSub)
  def Mul(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OMul)
  def Div(e1: Ex, e2: Ex): Ex = Bin(e1, e2, ODiv)
  def Pot(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OPot)

  def Equ(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OEq)
  def Neq(e1: Ex, e2: Ex): Ex = Bin(e1, e2, ONe)
  def Lt(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OLt)
  def Lte(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OLe)
  def Gt(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OGt)
  def Gte(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OGe)

  def And(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OAnd)
  def Or(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OOr)

  def Cons(e1: Ex, e2: Ex): Ex = Bin(e1, e2, OCons)
}

// Ternary operator
case class Tri(e1: Ex, e2: Ex, e3: Ex, op: Op3) extends Ex {
  override def infix = e1.infix + op.infix + e2.infix
}

object Tri {
  def SubStr(e1: Ex, e2: Ex, e3: Ex): Ex = Tri(e1, e2, e3, OSubStr)
}

case class Ife(e1: Ex, e2: Ex, e3: Ex) extends Ex {
  def infix = "if " + e1.infix + " then " + e2.infix + " else " + e3.infix
}

case class Fun(fargs: List[String], body: Ex) extends Ex {
  def infix = "fun " + fargs.mkString("(", ", ", ")") + " => " + body.infix
}

case class App(fexp: Ex, args: List[Ex]) extends Ex {
  def infix = (fexp match {
    case Var(_) => fexp.infix
    case _      => "(" + fexp.infix + ")"
  }) + (args map (_ infix)).mkString("(", ", ", ")");
}

case class LetR(fargs: List[String], args: List[Ex], body: Ex) extends Ex {
  def infix = "let* " + ((fargs zip args) map (p => { val (a, e) = p; a + "=" + e.infix })).mkString("; ") + " in " + body.infix
}

// For the REPL 
case class Def(n: String, e: Ex) extends Ex {
  def infix = "def " + n + "=" + e.infix
}

case class Undef(n: String) extends Ex {
  def infix = "undef " + n;
}

case class Load(n: String) extends Ex {
  def infix = "load " + n;
}

case class ReLoad() extends Ex {
  def infix = "reload"
}

case class Compile(e: Ex) extends Ex {
  def infix = "compile " + e.infix;
}

case class Comment(s: String) extends Ex {
  def infix = "// " + s;
}