/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.Env

abstract class Const extends Ex {
  override def eval(e: Env): Const = this
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
  protected implicit def booleanToConst(b: Boolean): Const = if (b) True else False
  protected implicit def bigintToConst(b: BigInt): Const = Z(b)
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

// For the REPL - to return a modified environment
case class ReplDef(n: String, c: Const) extends Const {
  override def infix = "define " + n + "=" + c
}

case class ReplUnDef(n: String) extends Const {
  override def infix = "undefine: " + n
}

