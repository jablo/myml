/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

object Sculla {
  val calc = new Parser()
  val e = Map[String, Const]();
  def check(p: calc.ParseResult[Ex]): calc.ParseResult[Ex] = {
    p match {
      case calc.Error(_, _) | calc.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }
}
