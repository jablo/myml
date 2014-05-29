/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.Env

abstract class Op(val infix: String, val mkEx: (Ex, Ex) => Ex, val eval: (Const, Const) => Const) extends ByteCode {
    def exec(stack: MStack, env: Env): Store = {
    val (v1, s1) = pop(stack)
    val (v2, s2) = pop(s1)
    (s2.push(eval(v1, v2)), env, none)
  }
}
abstract class UnOp(val infix: String, val mkEx: Ex => Ex, val eval: Const => Const) extends ByteCode {
    def exec(stack: MStack, env: Env): Store = {
    val (v1, s1) = pop(stack)
    (s1.push(eval(v1)), env, none)
  }
}

case object ONeg extends UnOp("-", Neg, -_)
case object OAdd extends Op("+", Add, _ + _)
case object OSub extends Op("-", Sub, _ - _)
case object OMul extends Op("*", Mul, _ * _)
case object ODiv extends Op("/", Div, _ / _)
case object OPot extends Op("^", Pot, _ ** _)

case object OEq extends Op("=", Equ, _ == _)
case object ONe extends Op("<>", Neq, _ != _)
case object OLt extends Op("<", Lt, _ < _)
case object OLe extends Op("<=", Lte, _ <= _)
case object OGt extends Op(">", Gt, _ > _)
case object OGe extends Op(">=", Gte, _ >= _)

// boolean
case object ONot extends UnOp("not ", Not, !_)
case object OAnd extends Op("and", And, _ && _)
case object OOr extends Op("or", Or, _ || _)

// List
case object OCons extends Op("::", Cons, ConsCell)
case object OCar extends UnOp("car", Car, (c) => c match {
  case ConsCell(a, _) => a
})

case object OCdr extends UnOp("cdr", Cdr, (c) => c match {
  case ConsCell(_, b) => b
})

