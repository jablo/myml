/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

object TestHelper {
  val calc = new Parser()
  val e = Map[String, Const]();
  def check(p: calc.ParseResult[Ex]): calc.ParseResult[Ex] = {
    p match {
      case calc.Error(_, _) | calc.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }
  def reparse(p: calc.ParseResult[Ex]): Ex = check(calc.parseAll(calc.expr, p.get.infix)).get
}