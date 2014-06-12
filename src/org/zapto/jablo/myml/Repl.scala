/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import scala.collection._
import scala.io.Source
import java.io.{ FileReader, FileNotFoundException, IOException }
import Ex.interp

object Repl {
  val env = BCMutEnv(NilScope(), mutable.Map[String, Const]())
  val calc = new Parser()

  def check(p: calc.ParseResult[Ex]): calc.ParseResult[Ex] = {
    p match {
      case calc.Error(_, _) | calc.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }

  def check2(p: calc.ParseResult[List[Ex]]): calc.ParseResult[List[Ex]] = {
    p match {
      case calc.Error(_, _) | calc.Failure(_, _) => print(p)
      case _                                     => Unit
    }
    p
  }

  def readfile(n: String): Unit = {
    val p: calc.ParseResult[List[Ex]] = calc.parseAll(calc.program, new FileReader(n))
    check2(p)
    p.get map (ep(_, env))
  }

  def readresource(n: String): Unit = {
    val preload = Source.fromURL(getClass.getResource(n))
    val p: calc.ParseResult[List[Ex]] = calc.parseAll(calc.program, preload.bufferedReader)
    check2(p)
    p.get map (ep(_, env))
  }

  def ep(exp: Ex, env: BCScope): Unit = {
    try {
      println("Parsed: " + exp)
      println(" Infix: " + (exp infix))
      val ev = ByteCodeMachine.interp(exp.bytecode, env) // interp(exp, env)
      println("Result: " + ev)
      println(" Infix: " + ev.infix)
      ev match {
        case ReplDef(n, c) => env += Pair(n, c)
        case ReplUnDef(n)  => env -= n
        case ReplLoad(n)   => readfile(n)
        case ReplReLoad() => {
          env.clear
          readresource("preload.myml")
        }
        case ReplRun(insns) => println("bcrun: " + ByteCodeMachine.interp(insns, env))
        case _              => Unit
      }
    } catch {
      case e: MyMLException => println(e.getMessage)
      case e: Throwable     => println(e); println(e.getStackTraceString)
    }
  }

  def repl: Unit = {
    Iterator.continually({ print("MyML> "); Console.readLine }).takeWhile((l) => l != null && l != "quit").
      foreach(line => {
        try {
          if (line.trim != "") {
            val p = calc.parseAll(calc.repl, line)
            check(p);
            ep(p.get, env)
          }
        } catch {
          case e: Throwable => println(e); println(e.getStackTraceString)
        }
      })
  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
//    readresource("preload.myml")
    repl
  }
}
