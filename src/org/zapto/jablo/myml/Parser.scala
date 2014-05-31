/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator._
import Q.q

class Parser extends JavaTokenParsers with PackratParsers {
  type ExPar = PackratParser[Ex]
  type OpPar = PackratParser[Op]
  type UnOpPar = PackratParser[UnOp]
  type EqPar = PackratParser[(String, Ex)]
  type ProgPar = PackratParser[List[Ex]]

  // The MyML language parser
  lazy val program: ProgPar = repsep(repl, ";") 
  lazy val expr: ExPar = error | cond | fun | let | letr | arith

  // REPL commands
  lazy val repl: ExPar = "def" ~> assignornamedfun ^^ ((p) => { val (a, b) = p; Def(a, b) }) |
    "undef" ~> ident ^^ ((p) => Undef(p)) |
    "load" ~> stringLiteral ^^ ((p)=>Load(stripQuote(p))) |
    "reload" ^^ ((_)=>ReLoad()) |
    "compile" ~> expr ^^ (Compile(_)) |
    "run" ~> expr ^^ (Run(_)) | expr 

  // Control structures
  lazy val error: ExPar = "error" ~ "(" ~> stringLiteral <~ ")" ^^ ((p:String)=>ErrorEx(stripQuote(p))) |
    "error" ~ "(" ~ ")" ^^ ((_)=>ErrorEx("")) | "error" ^^ ((_)=>ErrorEx(""))
    
  lazy val cond: ExPar = ("if" ~> arith <~ "then") ~ expr ~ ("else" ~> expr) ^^ {
    case test ~ yes ~ no => Ife(test, yes, no)
  }
  lazy val fun: ExPar = "fun" ~ "(" ~> repsep(ident, ",") ~ (")" ~ "=>" ~> expr) ^^ {
    case vs ~ b => Fun(vs, b)
  } | "fun" ~> namedfun ^^ { case (fnam, args, body) => LetR(List(fnam), List(Fun(args, body)), Var(fnam)) }
  lazy val namedfun: PackratParser[(String,List[String],Ex)] = ident ~ ( "(" ~> repsep(ident, ",")) ~ (")" ~ "=>" ~> expr) ^^ {
    case fnam ~ vs ~ b => (fnam,vs,b)
  }
  lazy val let: ExPar = "let" ~> repsep(assign, ";") ~ ("in" ~> expr) ^^ {
    case asgns ~ body => {
      val (args, exprs) = asgns.unzip
      App(Fun(args, body), exprs)
    }
  } 
  lazy val letr: ExPar = "let*" ~> repsep(assignornamedfun, ";") ~ ("in" ~> expr) ^^ {
    case asgns ~ body => {
      val (args, exprs) = asgns.unzip
      LetR(args, exprs, body)
    } 
  } 
  lazy val assign: EqPar = ident ~ ("=" ~> expr) ^^ {
    case v ~ e => (v, e)
  } 
  lazy val assignornamedfun: EqPar = assign | namedfun ^^ { case (fnam, args, body) => (fnam, Fun(args,body))}
  lazy val builtin: ExPar = "car" ~ "(" ~> expr <~ ")" ^^ (Car) | "cdr" ~ "(" ~> expr <~ ")" ^^ (Cdr) |
    "substr" ~ "(" ~> repN(3, expr) <~ ")" ^^ { case List(e1, e2, e3) => SubStr(e1, e2, e3) } |
    "strlen" ~ "(" ~> expr <~ ")" ^^ (StrLen) |
    "tostr"  ~ "(" ~> expr <~ ")" ^^ (ToStr) |
    "trim" ~ "(" ~> expr <~ ")" ^^ (TrimStr)

  // Arithmetic expressions
  lazy val arith: ExPar = cmp

  lazy val cmp: ExPar = sum ~ cmpop ~ sum ^^ {
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
  } | listop

  lazy val listop: ExPar = listop ~ ("::" ~> listop) ^^ {
    case e1 ~ e2 => Cons(e1, e2)
  } | sum
  lazy val sum: ExPar = sum ~ sumop ~ product ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | product
  lazy val product: ExPar = product ~ mulop ~ pow ^^ {
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | pow
  lazy val pow: ExPar = term ~ powop ~ pow ^^ { // note: pow (^) is right-associative
    case e1 ~ op ~ e2 => op.mkEx(e1, e2)
  } | unary
  lazy val unary: ExPar = negop ~ unary ^^ {
    case op ~ e => op.mkEx(e)
  } | term
  lazy val term: ExPar = app | num | variable | pexpr
  lazy val app: ExPar = builtin | (app | variable | pexpr2) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case fn ~ args => App(fn, args)
  }
  lazy val pexpr: ExPar = "(" ~> expr <~ ")" ^^ ((f) => Par(f))
  lazy val pexpr2: ExPar = "(" ~> expr <~ ")"

  // Comparison Operator terminals
  lazy val cmpop: OpPar = "<>" ^^ ((_) => ONe) | ">=" ^^ ((_) => OGe) | "<=" ^^ ((_) => OLe) |
    "<" ^^ ((_) => OLt) | ">" ^^ ((_) => OGt) | "=" ^^ ((_) => OEq)

  // Arithmetic Operator terminals
  lazy val sumop: OpPar = "+" ^^ ((_) => OAdd) | "-" ^^ ((_) => OSub)
  lazy val mulop: OpPar = "*" ^^ ((_) => OMul) | "/" ^^ ((_) => ODiv)
  lazy val powop: OpPar = "^" ^^ ((_) => OPot)
  lazy val negop: UnOpPar = "-" ^^ ((_) => ONeg)

  // Boolean operator terminals
  lazy val orop: OpPar = "or" ^^ ((_) => OOr)
  lazy val andop: OpPar = "and" ^^ ((_) => OAnd)
  lazy val notop: UnOpPar = "not" ^^ ((_) => ONot)

  // Terminals
  lazy val num: ExPar = wholeNumber ~ ("/" ~> wholeNumber) ^^ { case n ~ d => q(BigInt(n), BigInt(d)) } |
    wholeNumber ^^ ((p) => Z(BigInt(p))) |
    "true" ^^ ((_) => True) | "false" ^^ ((_) => False) | "nil" ^^ ((_) => MNil) | stringLiteral ^^ Str    
  lazy val variable: ExPar = ident ^^ ((p) => Var(p))
  
  def stripQuote(p:String):String = p drop(1) dropRight(1)
}
