/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers with PackratParsers {
  type ExPar = PackratParser[Ex]
  type OpPar = PackratParser[Op]
  type UnOpPar = PackratParser[UnOp]
  type EqPar = PackratParser[(String, Ex)]

  lazy val expr: ExPar = cond | fun | let | letr | arith

  // Control structures
  lazy val cond: ExPar = ("if" ~> arith <~ "then") ~ expr ~ ("else" ~> expr) ^^ {
    case test ~ yes ~ no => Ife(test, yes, no)
  }
  lazy val fun: ExPar = "fun" ~ "(" ~> repsep(ident, ",") ~ (")" ~ "=>" ~> expr) ^^ {
    case vs ~ b => Fun(vs, b)
  }
  lazy val let: ExPar = "let" ~> repsep(assign, ";") ~ ("in" ~> expr) ^^ {
    case asgns ~ body => {
      val (args, exprs) = asgns.unzip
      App(Fun(args, body), exprs)
    }
  }
  lazy val letr: ExPar = "let*" ~> repsep(assign, ";") ~ ("in" ~> expr) ^^ {
    case asgns ~ body => {
      val (args, exprs) = asgns.unzip
      LetR(args, exprs, body)
    }
  }
  lazy val assign: EqPar = ident ~ ("=" ~> expr) ^^ {
    case v ~ e => (v, e)
  }

  // Arithmetic expressions
  lazy val arith: ExPar = cmp

  lazy val cmp: ExPar = cmp ~ cmpop ~ cmp ^^ {
    case a ~ op ~ b => op.mkEx(a, b)
  } | boolsum

  lazy val boolsum: ExPar = boolsum ~ orop ~ boolprod ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | boolprod
  lazy val boolprod: ExPar = boolsum ~ andop ~ boolprod ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | boolun
  lazy val boolun: ExPar = notop ~ boolun ^^ {
    case op ~ e => op.mkEx(e)
  } | sum
  
  lazy val sum: ExPar = sum ~ sumop ~ product ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | product
  lazy val product: ExPar = product ~ mulop ~ pow ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | pow
  lazy val pow: ExPar = pow ~ powop ~ term ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | unary
  lazy val unary: ExPar = negop ~ unary ^^ {
    case op ~ e => op.mkEx(e)
  } | term
  lazy val term: ExPar = app | num | variable | pexpr
  lazy val app: ExPar = (app | variable | pexpr) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case fn ~ args => App(fn, args)
  }
  lazy val pexpr: ExPar = "(" ~> expr <~ ")" ^^ ((f) => Par(f))

  // Arithmetic Operator terminals
  lazy val cmpop: OpPar = "<>" ^^ ((_) => ONe) | ">=" ^^ ((_) => OGe) | "<=" ^^ ((_) => OLe) |
    "<" ^^ ((_) => OLt) | ">" ^^ ((_) => OGt) | "=" ^^ ((_) => OEq)
  lazy val sumop: OpPar = "+" ^^ ((_) => OAdd) | "-" ^^ ((_) => OSub)
  lazy val mulop: OpPar = "*" ^^ ((_) => OMul) | "/" ^^ ((_) => ODiv)
  lazy val powop: OpPar = "^" ^^ ((_) => OPot)
  lazy val negop: UnOpPar = "-" ^^ ((_) => ONeg)

  // Boolean operator terminals
  lazy val orop: OpPar = "or" ^^ ((_) => OOr)
  lazy val andop: OpPar = "and" ^^ ((_) => OAnd)
  lazy val notop: UnOpPar = "not" ^^ ((_) => ONot)

  // Terminals
  lazy val num: ExPar = wholeNumber ~ ("/" ~> wholeNumber) ^^ {
    case n ~ d => Q(Integer.parseInt(n), Integer.parseInt(d))
  } |
    wholeNumber ^^ ((p) => Z(Integer.parseInt(p))) |
    "true" ^^ ((_) => True) | "false" ^^ ((_) => False)
  lazy val variable: ExPar = ident ^^ ((p) => Var(p))
}
