/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

abstract class Op(s: String, mk: (Ex,Ex)=>Ex) {
  def eval(c1: Const, c2: Const): Const
  def mkEx = mk
  def infix: String = s
}
abstract class UnOp(s: String, mk: Ex=>Ex) {
  def eval(c1: Const): Const
  def mkEx = mk
  def infix: String = s
}

case object ONeg extends UnOp("-", Neg) {
  override def eval(c1: Const): Const = -c1
}

case object OAdd extends Op("+", Add) {
  override def eval(c1: Const, c2: Const): Const = c1 + c2
}

case object OSub extends Op("-", Sub) {
  override def eval(c1: Const, c2: Const): Const = c1 - c2
}

case object OMul extends Op("*", Mul) {
  override def eval(c1: Const, c2: Const): Const = c1 * c2
}

case object ODiv extends Op("/", Div) {
  override def eval(c1: Const, c2: Const): Const = c1 / c2
}

case object OPot extends Op("^", Pot) {
  override def eval(c1: Const, c2: Const): Const = c1 ** c2
}

case object OEq extends Op("=", Equ) {
  override def eval(c1: Const, c2: Const): Const = c1 == c2
}

case object ONe extends Op("<>", Neq) {
  override def eval(c1: Const, c2: Const): Const = c1 != c2
}

case object OLt extends Op("<", Lt) {
  override def eval(c1: Const, c2: Const): Const = c1 < c2
}

case object OLe extends Op("<=", Lte) {
  override def eval(c1: Const, c2: Const): Const = c1 <= c2
}

case object OGt extends Op(">", Gt) {
  override def eval(c1: Const, c2: Const): Const = c1 > c2
}

case object OGe extends Op(">=", Gte) {
  override def eval(c1: Const, c2: Const): Const = c1 >= c2
}

// boolean

case object ONot extends UnOp("not ", Not) {
  override def eval(c1: Const): Const = !c1
}

case object OAnd extends Op("and", And) {
  override def eval(c1: Const, c2: Const): Const = c1 && c2
}

case object OOr extends Op("or", Or) {
  override def eval(c1: Const, c2: Const): Const = c1 || c2
}
