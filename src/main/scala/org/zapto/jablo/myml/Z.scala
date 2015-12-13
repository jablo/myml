/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Const.pow
import Q.q

case class Z(i: BigInt) extends Const {
  override def unary_- : Const = Z(-i)
  override def +(c: Const): Const = c match {
    case Z(j)    => Z(i + j)
    case Q(n, d) => q(n, d) + q(i, 1)
  }
  override def -(c: Const): Const = c match {
    case Z(j)    => Z(i - j)
    case Q(n, d) => q(n, d) - q(i, 1)
  }
  override def *(c: Const): Const = c match {
    case Z(j)    => Z(i * j)
    case Q(n, d) => q(n, d) * q(i, 1)
  }
  override def /(c: Const): Const = c match {
    case Z(j)    => q(i, j)
    case Q(n, d) => q(i * d, n)
  }
  override def **(c: Const): Const = c match {
    case Z(j) => Z(pow(i, j))
    case _    => undef("^", c)
  }
  // comparison operations
  override def ==(c: Const): Const = c match {
    case Z(j)        => i == j
    case c @ Q(n, d) => q(i, 1) == c
    case _           => false
  }
  override def !=(c: Const): Const = c match {
    case Z(j)        => i != j
    case c @ Q(n, d) => q(i, 1) != c
    case _           => true
  }
  override def <(c: Const): Const = c match {
    case Z(j)        => i < j
    case c @ Q(n, d) => q(i, 1) < c
    case _           => false
  }
  override def <=(c: Const): Const = c match {
    case Z(j)        => i <= j
    case c @ Q(n, d) => q(i, 1) <= c
    case _           => false
  }
  override def >(c: Const): Const = c match {
    case Z(j)        => i > j
    case c @ Q(n, d) => q(i, 1) > c
    case _           => false
  }
  override def >=(c: Const): Const = c match {
    case Z(j)        => i >= j
    case c @ Q(n, d) => q(i, 1) >= c
    case _           => false
  }
  override def infix = i.toString
}

