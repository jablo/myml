/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

object Repl {
  var env = scala.collection.Map[String, Const]()
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
    Iterator.continually({ print("MyML> "); Console.readLine }).takeWhile((l) => l != null && l != "quit").
      foreach(line => {
        try {
          val p = calc.parseAll(calc.repl, line)
          check(p);
          val exp = p.get
          println("Parsed: " + exp)
          println(" Infix: " + (exp infix))
          val ev = exp eval env
          println("Result: " + ev)
          println(" Infix:" + ev.infix)
          ev match {
            case Defs(e) => env = e
            case _       => Unit
          }
        } catch {
          case e: Exception => println(e)
        }
      })
  }
}
