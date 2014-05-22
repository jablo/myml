/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

sealed trait Op {
  def eval(c1: Const, c2: Const): Const
  def mkEx(e1: Ex, e2: Ex): Ex
  def infix: String
}
sealed trait UnOp {
  def eval(c1: Const): Const
  def mkEx(e1: Ex): Ex
  def infix: String
}

case object ONeg extends UnOp {
  override def eval(c1: Const): Const = -c1
  override def mkEx(e1: Ex): Ex = Neg(e1)
  override def infix = "not "
}

case object OAdd extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 + c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Add(e1, e2)
  override def infix = "+"
}

case object OSub extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 - c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Sub(e1, e2)
  override def infix = "-"
}

case object OMul extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 * c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Mul(e1, e2)
  override def infix = "*"
}

case object ODiv extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 / c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Div(e1, e2)
  override def infix = "/"
}

case object OPot extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 ** c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Pot(e1, e2)
  override def infix = "^"
}

case object OEq extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 == c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Equ(e1, e2)
  override def infix = "="
}

case object ONe extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 != c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Neq(e1, e2)
  override def infix = "="
}

case object OLt extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 < c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Lt(e1, e2)
  override def infix = "="
}

case object OLe extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 <= c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Lte(e1, e2)
  override def infix = "="
}

case object OGt extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 > c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Gt(e1, e2)
  override def infix = "="
}

case object OGe extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 >= c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Gte(e1, e2)
  override def infix = "="
}

// boolean

case object ONot extends UnOp {
  override def eval(c1: Const): Const = !c1
  override def mkEx(e1: Ex): Ex = Not(e1)
  override def infix = "not "
}

case object OAnd extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 && c2
  override def mkEx(e1: Ex, e2: Ex): Ex = And(e1, e2)
  override def infix = "="
}

case object OOr extends Op {
  override def eval(c1: Const, c2: Const): Const = c1 || c2
  override def mkEx(e1: Ex, e2: Ex): Ex = Or(e1, e2)
  override def infix = "="
}
