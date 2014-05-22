/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

object Repl {
  val env = scala.collection.mutable.Map[String, Const]()
  val calc = new Parser()

  def check(p: calc.ParseResult[Ex]): calc.ParseResult[Ex] = {
    p match {
      case calc.Error(_, _) | calc.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    Iterator.continually({ print("MyML> "); Console.readLine }).takeWhile(_ != null).
      foreach(line => {
        val p = calc.parseAll(calc.expr, line)
        check(p);
        println(p.get.eval(env))
      })
  }
}
