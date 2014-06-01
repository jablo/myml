/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Ex.{ Env, typerr }

// Cheat a bit and let the operators fill in as ByteCode instructions too, sharing the definitions betwee
// abstract syntax tree interpreter and the bytecode interpreter.

abstract class Op(val infix: String, val mkEx: (Ex, Ex) => Ex, val eval: (Const, Const) => Const) extends ByteCode with ExHelper {
  // bytecode intepretation  of the operator
  final def exec(stack: MStack, env: BCScope): Store = {
    val (v2, s1) = pop(stack)
    val (v1, s2) = pop(s1)
    (s2.push(eval(v1, v2)), env, none)
  }
}
abstract class UnOp(val infix: String, val mkEx: Ex => Ex, val eval: Const => Const) extends ByteCode {
  // bytecode intepretation  of the operator
  final def exec(stack: MStack, env: BCScope): Store = {
    val (v1, s1) = pop(stack)
    (s1.push(eval(v1)), env, none)
  }
}
abstract class Op3(val infix: String, val mkEx: (Ex, Ex, Ex) => Ex, val eval: (Const, Const, Const) => Const) extends ByteCode {
  // bytecode intepretation  of the operator
  final def exec(stack: MStack, env: BCScope): Store = {
    val (v3, s1) = pop(stack)
    val (v2, s2) = pop(stack)
    val (v1, s3) = pop(s1)
    (s2.push(eval(v1, v2, v3)), env, none)
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

case object OSubStr extends Op3("substr", SubStr, (a, b, c) => {
  val s: String = a match {
    case Str(s) => s
    case _      => typerr("Expected string", a)
  }
  val from: Int = b match {
    case Z(b) => b.intValue
    case _    => typerr("Expected int", b)
  }
  val to: Int = b match {
    case Z(c) => c.intValue
    case _    => typerr("Expected int", c)
  }
  Str(s.substring(from, to))
})

case object OTrimStr extends UnOp("trim", TrimStr, (c) => {
  c match {
    case Str(s) => Str(s.trim)
    case _      => typerr("Expected string", c)
  }
})
case object OStrLen extends UnOp("strlen", StrLen, (c) => {
  c match {
    case Str(s) => Z(s.length)
    case _      => typerr("Expected string", c)
  }
})

case object OToStr extends UnOp("tostr", ToStr, (p) => Str(p.infix))
