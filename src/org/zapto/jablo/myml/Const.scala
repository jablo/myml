/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.Env

abstract class Const extends Ex {
  override def eval(e: Env): Const = this
  def +(c: Const): Const = throw new RuntimeException("Illegal operation")
  def -(c: Const): Const = throw new RuntimeException("Illegal operation")
  def *(c: Const): Const = throw new RuntimeException("Illegal operation")
  def /(c: Const): Const = throw new RuntimeException("Illegal operation")
  def **(c: Const): Const = throw new RuntimeException("Illegal operation")
  def ==(c: Const): Const = throw new RuntimeException("Illegal operation")
  def !=(c: Const): Const = throw new RuntimeException("Illegal operation")
  def <(c: Const): Const = throw new RuntimeException("Illegal operation")
  def <=(c: Const): Const = throw new RuntimeException("Illegal operation")
  def >(c: Const): Const = throw new RuntimeException("Illegal operation")
  def >=(c: Const): Const = throw new RuntimeException("Illegal operation")
  def &&(c: Const): Const = throw new RuntimeException("Illegal operation")
  def ||(c: Const): Const = throw new RuntimeException("Illegal operation")
  def unary_! : Const = throw new RuntimeException("Illegal operation")
  def unary_- : Const = throw new RuntimeException("Illegal operation")
  protected final def pow(b: Int, ex: Int): Int =
    if (ex < 0) {
      require(b != 0)
      if (b == 1) 1
      else if (b == -1)
        if ((ex & 1) == 0) -1 else 1
      else 0
    } else {
      _pow(1, b, ex)
    }
  protected final def _pow(t: Int, b: Int, e: Int): Int = {
    if (e == 0) t
    else if ((e & 1) == 1)
      _pow(t * b, b * b, e >> 1)
    else
      _pow(t, b * b, e >> 1)
  }
  protected final def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
  def mkBool(b: Boolean): Const = if (b) True else False
}

case class Z(i: Int) extends Const {
  override def unary_- : Const = Z(-i)
  override def +(c: Const): Const = c match {
    case Z(j)    => Z(i + j)
    case Q(n, d) => Q(n, d) + Q(i, 1)
  }
  override def -(c: Const): Const = c match {
    case Z(j)    => Z(i - j)
    case Q(n, d) => Q(n, d) - Q(i, 1)
  }
  override def *(c: Const): Const = c match {
    case Z(j)    => Z(i * j)
    case Q(n, d) => Q(n, d) * Q(i, 1)
  }
  override def /(c: Const): Const = c match {
    case Z(j)    => Q(i, j)
    case Q(n, d) => Q(i * d, n)
  }
  override def **(c: Const): Const = c match {
    case Z(j) => Z(pow(i, j))
    case _    => throw new RuntimeException("Power only supported for whole numbers")
  }
  // comparison operations
  override def ==(c: Const): Const = c match {
    case Z(j) => mkBool(i == j)
    case _    => mkBool(false)
  }
  override def !=(c: Const): Const = c match {
    case Z(j) => mkBool(i != j)
    case _    => mkBool(true)
  }
  override def <(c: Const): Const = c match {
    case Z(j) => mkBool(i < j)
    case _    => mkBool(false)
  }
  override def <=(c: Const): Const = c match {
    case Z(j) => mkBool(i <= j)
    case _    => mkBool(false)
  }
  override def >(c: Const): Const = c match {
    case Z(j) => mkBool(i > j)
    case _    => mkBool(false)
  }
  override def >=(c: Const): Const = c match {
    case Z(j) => mkBool(i >= j)
    case _    => mkBool(false)
  }
  override def infix = i.toString
}

case object True extends Const {
  override def ==(c: Const): Const = c match {
    case True => True
    case _    => False
  }
  override def !=(c: Const): Const = c match {
    case True => False
    case _    => True
  }
  override def &&(c: Const): Const = c match {
    case True => True
    case _    => False
  }
  override def ||(c: Const): Const = True
  override def unary_! : Const = False
  override def infix: String = "true"
}

case object False extends Const {
  override def ==(c: Const): Const = c match {
    case False => True
    case _     => False
  }
  override def !=(c: Const): Const = c match {
    case False => False
    case _     => True
  }
  override def &&(c: Const): Const = False
  override def ||(c: Const): Const = c match {
    case True => True
    case _    => False
  }
  override def unary_! : Const = True
  override def infix: String = "false"
}

case class Q(n: Int, d: Int) extends Const {
  require(d != 0)
  private val g = gcd(n, d)
  val numer: Int = n / g
  val denom: Int = d / g
  override def unary_- : Const = Q(-numer, denom)
  override def +(t: Const) = t match {
    case that @ Z(n)      => this + Q(n, 1)
    case that @ Q(tn, td) => Q(numer * that.denom + that.numer * denom, denom * that.denom)
  }
  override def -(t: Const) = t match {
    case that @ Z(n)      => this - Q(n, 1)
    case that @ Q(tn, td) => Q(numer * that.denom - that.numer * denom, denom * that.denom)
  }
  override def *(t: Const) = t match {
    case that @ Z(n)      => this * Q(n, 1)
    case that @ Q(tn, td) => Q(numer * that.numer, denom * that.denom)
  }
  override def /(t: Const) = t match {
    case that @ Z(n)      => this / Q(n, 1)
    case that @ Q(tn, td) => Q(numer * that.denom, denom * that.numer)
  }
  override def **(t: Const) = t match {
    case that @ Z(n)    => Q(pow(numer, n), pow(denom, n))
    case that @ Q(_, _) => throw new RuntimeException("Pow only for whole powers")
  }
  override def infix = n.toString + "/" + d.toString
  override def equals(o: Any) = o match {
    case q @ Q(_, _) => q.numer == numer && q.denom == denom
    case _           => false
  }
  override def hashCode = 13 * numer.hashCode * denom.hashCode;
}

