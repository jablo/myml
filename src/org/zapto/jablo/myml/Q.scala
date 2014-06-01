/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Const.{ gcd, pow }
import Q.q
import Ex.{ err, typerr}

case class Q(n: BigInt, d: BigInt) extends Const {
  require(d != 0)
  private val g = gcd(n, d)
  val numer: BigInt = n / g
  val denom: BigInt = d / g
  override def unary_- : Const = q(-numer, denom)
  override def +(t: Const) = t match {
    case that @ Z(n)      => this + q(n, 1)
    case that @ Q(tn, td) => q(numer * that.denom + that.numer * denom, denom * that.denom)
  }
  override def -(t: Const) = t match {
    case that @ Z(n)      => this - q(n, 1)
    case that @ Q(tn, td) => q(numer * that.denom - that.numer * denom, denom * that.denom)
  }
  override def *(t: Const) = t match {
    case that @ Z(n)      => this * q(n, 1)
    case that @ Q(tn, td) => q(numer * that.numer, denom * that.denom)
  }
  override def /(t: Const) = t match {
    case that @ Z(n)      => this / q(n, 1)
    case that @ Q(tn, td) => q(numer * that.denom, denom * that.numer)
  }
  override def **(t: Const) = t match {
    case that @ Z(n)    => q(pow(numer, n), pow(denom, n))
    case that @ Q(_, _) => throw new RuntimeException("Pow only for whole powers")
  }

  override def ==(c: Const): Const = c match {
    case Z(j)        => equals(q(j, 1))
    case c @ Q(_, _) => equals(c)
    case _           => typerr("Expected Z or Q", c)
  }
  override def !=(c: Const): Const = c match {
    case Z(j)        => !equals(q(j, 1))
    case c @ Q(n, d) => !equals(c)
    case _           => typerr("Expected Z or Q", c)
  }
  override def <(c: Const): Const = c match {
    case Z(j)          => n * 1 < j * d
    case c @ Q(n1, d1) => n * d1 < n1 * d
    case _             => typerr("Expected Z or Q", c)
  }
  override def <=(c: Const): Const = c match {
    case Z(j)          => n * 1 <= j * d
    case c @ Q(n1, d1) => n * d1 <= n1 * d
    case _             => typerr("Expected Z or Q", c)
  }
  override def >(c: Const): Const = c match {
    case Z(j)          => n * 1 > j * d
    case c @ Q(n1, d1) => n * d1 > n1 * d
    case _             => typerr("Expected Z or Q", c)
  }
  override def >=(c: Const): Const = c match {
    case Z(j)          => n * 1 >= j * d
    case c @ Q(n1, d1) => n * d1 >= n1 * d
    case _             => typerr("Expected Z or Q", c)
  }
  override def infix = numer.toString + "/" + denom.toString
  override def equals(o: Any) = o match {
    case q @ Q(_, _) => q.numer == numer && q.denom == denom
    case _           => false
  }
  override def hashCode = 13 * numer.hashCode * denom.hashCode;
}

object Q {
  def q(n: BigInt, d: BigInt): Q = {
    val g = gcd(n, d)
    Q(n / g, d / g)
  }
}