/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import scala.language.postfixOps
import Ex.Env

abstract class Const extends Ex {
  protected def undef(op: String, c: Const): Nothing = throw new UndefinedOperationException("Undefined operation " + this + op + c)
  protected def undef(op: String): Nothing = throw new UndefinedOperationException("Undefined operation " + op + " " + this)
  def unary_- : Const = undef("-")
  def +(c: Const): Const = undef("+", c);
  def -(c: Const): Const = undef("-", c)
  def *(c: Const): Const = undef("*", c)
  def /(c: Const): Const = undef("/", c)
  def **(c: Const): Const = undef("^", c)
  def ==(c: Const): Const = undef("=", c)
  def !=(c: Const): Const = undef("<>", c)
  def <(c: Const): Const = undef("<", c)
  def <=(c: Const): Const = undef("<=", c)
  def >(c: Const): Const = undef(">", c)
  def >=(c: Const): Const = undef(">=", c)
  def unary_! : Const = undef("not")
  def &&(c: Const): Const = undef("and", c)
  def ||(c: Const): Const = undef("or", c)
}

object Const {
  final def pow(b: BigInt, ex: BigInt): BigInt =
    if (ex < 0) {
      require(b != 0)
      if (b == 1) 1
      else if (b == -1)
        if ((ex & 1) == 0) -1 else 1
      else 0
    } else {
      _pow(1, b, ex)
    }
  final def _pow(t: BigInt, b: BigInt, e: BigInt): BigInt = {
    if (e == 0) t
    else if ((e & 1) == 1)
      _pow(t * b, b * b, e >> 1)
    else
      _pow(t, b * b, e >> 1)
  }
  final def gcd(x: BigInt, y: BigInt): BigInt = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
}

// The empty value
case object MVoid extends Const {
  override def infix: String = "void"
  override def ==(c: Const): Const = MVoid
  override def !=(c: Const): Const = MVoid
}

// For list representation
case object MNil extends Const {
  override def infix: String = "nil"
  override def ==(c: Const): Const = c match {
    case MNil           => True
    case ConsCell(_, _) => False
    case _              => undef("=", c)
  }
  override def !=(c: Const): Const = c match {
    case MNil           => False
    case ConsCell(_, _) => True
    case _              => undef("<>", c)
  }
}

case class ConsCell(c1: Const, c2: Const) extends Const {
  override def infix: String = c1 match {
    case ConsCell(c11, _) => "[" + c1.infix + "]" + "::" + c2.infix
    case _                => c1.infix + "::" + c2.infix
  }
  override def ==(c: Const): Const = c match {
    case MNil           => False
    case ConsCell(a, b) => c1 == a && c2 == b
    case _              => undef("=", c)
  }
  override def !=(c: Const): Const = c match {
    case MNil           => True
    case ConsCell(a, b) => c1 != a || c2 != b
    case _              => undef("<>", c)
  }
}

case class Str(s: String) extends Const {
  override def infix: String = s
  override def +(c: Const): Const = c match {
    case Str(t) => Str(s + t)
    case _      => undef("+", c)
  }
  override def ==(c: Const): Const = c match {
    case Str(t) => s == t
    case _      => undef("=", c)
  }
  override def !=(c: Const): Const = c match {
    case Str(t) => s != t
    case _      => undef("<>", c)
  }
  override def <(c: Const): Const = c match {
    case Str(t) => s < t
    case _      => undef("<", c)
  }
  override def <=(c: Const): Const = c match {
    case Str(t) => s <= t
    case _      => undef("<=", c)
  }
  override def >(c: Const): Const = c match {
    case Str(t) => s > t
    case _      => undef(">", c)
  }
  override def >=(c: Const): Const = c match {
    case Str(t) => s >= t
    case _      => undef(">=", c)
  }
}

// Closures for the direct interpreter
case class Clo(fargs: List[String], body: Ex, env: BCEnv) extends Const {
  // avoid printing environment values - letrec creates cyclic environment
  override def infix = Fun(fargs, body).infix + "@" + (env keys).mkString("{", ",", "}")
  override def toString = "Clo(" + fargs + ", " + body + ", " + (env keys).mkString("{", ",", "}") + ")"
}

// Closures for the bytecode interpreter
case class Subr(fargs: List[String], code: List[ByteCode], env: BCEnv) extends Const {
  // avoid printing environment values - letrec creates cyclic environment
  override def infix = fargs.mkString("[", ",", "] ") + code.mkString("[", ",", "]")
  override def toString = "Subr(" + fargs.mkString("[", ",", "], ") + code.mkString("[", ",", "] @ ") + env.keys.mkString("{", ", ", "}")
}

// then/else branches on the stack and the initial program for the bytecode interpreter
case class Code(ins: List[ByteCode]) extends Const {
  override def infix = "not implemented"
}

// Run bytecode through the bytecode interpreter
case class ReplRun(ins: List[ByteCode]) extends Const {
  override def infix = "not implemented"
}

// For the REPL - to return a modified environment
case class ReplDef(n: String, c: Const) extends Const {
  override def infix = "define " + n + "=" + c
}

case class ReplUnDef(n: String) extends Const {
  override def infix = "undefine: " + n
}

case class ReplLoad(n: String) extends Const {
  override def infix = "load " + n
}

case class ReplReLoad() extends Const {
  override def infix = "reload"
}
