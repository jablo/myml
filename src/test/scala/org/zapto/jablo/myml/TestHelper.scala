/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

import org.scalatest.FunSuite

class TestHelper extends FunSuite {
  val parser = new Parser()
  val e = BCNilEnv //Map[String, Const]();
  def eval(p: Ex, e: Map[String,Const]): Const = ByteCodeMachine.interp(Compiler.compile(p), BCNilEnv ++ e.toList)
  def report(p: parser.ParseResult[Ex]): parser.ParseResult[Ex] = {
    p match {
      case parser.Error(_, _) | parser.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }
  def reparse(p: parser.ParseResult[Ex]): Ex = parser.parseAll(parser.expr, p.get.infix).get
}
