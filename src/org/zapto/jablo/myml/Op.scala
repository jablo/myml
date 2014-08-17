/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import Un._
import Bin._
import Tri._

// Cheat a bit and let the operators fill in as ByteCode instructions too, sharing the definitions betwee
// abstract syntax tree interpreter and the bytecode interpreter.

case class Op(val infix: String, val mkEx: (Ex, Ex) => Ex, val eval: (Const, Const) => Const) extends ByteCode {
  final def exec(stack: MStack, env: BCEnv): Store = {
    stack match {
      case v2 :: v1 :: s2 => (eval(v1, v2) :: s2, env, none)
    }
  }
}

object Op extends ExHelper {
  val OAdd = Op("+", Add, _ + _)
  val OSub = Op("-", Sub, _ - _)
  val OMul = Op("*", Mul, _ * _)
  val ODiv = Op("/", Div, _ / _)
  val OPot = Op("^", Pot, _ ** _)
  val OEq = Op("=", Equ, _ == _)
  val ONe = Op("<>", Neq, _ != _)
  val OLt = Op("<", Lt, _ < _)
  val OLe = Op("<=", Lte, _ <= _)
  val OGt = Op(">", Gt, _ > _)
  val OGe = Op(">=", Gte, _ >= _)
  val OAnd = Op("and", And, _ && _)
  val OOr = Op("or", Or, _ || _)
  val OCons = Op("::", Cons, ConsCell)
}

case class UnOp(val infix: String, val mkEx: Ex => Ex, val eval: Const => Const) extends ByteCode {
  final def exec(stack: MStack, env: BCEnv): Store = {
    stack match {
      case v1 :: s1 => (eval(v1) :: s1, env, none)
    }
  }
}

object UnOp extends ExHelper {
  val ONeg = UnOp("-", Neg, -_)
  val ONot = UnOp("not ", Not, !_)
  val OCar = UnOp("car", Car, (c) => c match {
      case ConsCell(a, _) => a
    })
  val OCdr = UnOp("cdr", Cdr, (c) => c match {
      case ConsCell(_, b) => b
    })
  val OTrimStr = UnOp("trim", TrimStr, (c) => {
      c match {
        case Str(s) => Str(s.trim)
        case _      => typerr("Expected string", c)
      }
    })
  val OStrLen = UnOp("strlen", StrLen, (c) => {
      c match {
        case Str(s) => Z(s.length)
        case _      => typerr("Expected string", c)
      }
    })
  val OToStr = UnOp("tostr", ToStr, (p) => Str(p.infix))
}

case class Op3(val infix: String, val mkEx: (Ex, Ex, Ex) => Ex, val eval: (Const, Const, Const) => Const) extends ByteCode {
  final def exec(stack: MStack, env: BCEnv): Store = {
    stack match {
      case v3 :: v2 :: v1 :: s3 => (eval(v1, v2, v3) :: s3, env, none)
    }
  }
}

object Op3 extends ExHelper {
  val OSubStr = Op3("substr", SubStr, (a, b, c) => {
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
}
