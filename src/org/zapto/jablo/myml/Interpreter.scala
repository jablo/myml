/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import scala.collection._
import org.zapto.jablo.myml.Ex._
import scala.annotation.tailrec

// A single step intepretation result - a constant or a new expression and environment to evaluate
abstract sealed class EvalStep
case class Done(r: Const) extends EvalStep
case class Next(e: Ex, env: Env) extends EvalStep

object Interpreter extends ExHelper {
  @tailrec
  final def interp(e: Ex, env: Env): Const = {
    step(e, env) match {
      case Done(r)        => r
      case Next(e1, env1) => interp(e1, env1)
    }
  }
  def step(e: Ex, env: Env): EvalStep = {
    e match {
      case ErrorEx(n) => throw new MyMLException("Error: " + n)
      case Var(n) => env get n match {
        case None    => err("Unknown variable", e)
        case Some(v) => Next(v, env)
      }
      case Par(e1)         => step(e1, env)
      case Un(e1, op)      => Done(op.eval(interp(e1, env)))
      case Bin(e1, e2, op) => Done(op.eval(interp(e1, env), interp(e2, env)))
      case Ife(e1, e2, e3) => {
        val test = interp(e1, env)
        test match {
          case True  => Next(e2, env)
          case False => Next(e3, env)
          case _     => typerr("boolean", e1)
        }
      }
      case Fun(fargs, body) => Done(Clo(fargs, body, env))
      case App(fexp, args) => {
        val fun = interp(fexp, env)
        fun match {
          case _@ Clo(fargs, body, fenv) =>
            val actarg = args map ((arg: Ex) => interp(arg, env))
            val env2 = fenv ++ Map(fargs zip actarg: _*)
            Next(body, env2)
          case _ => typerr("Not a function", fexp)
        }
      }
      case LetR(fargs, args, body) => {
        // Use a mutable map initialized with current env so we can evaluate arguments in their own environmnent, creating a cyclic environment
        val letrecenv = mutable.Map[String, Const](env toList: _*)
        val actarg = args map (interp(_, letrecenv));
        letrecenv ++= fargs zip actarg
        Next(body, letrecenv)
      }
      case Def(n, e)  => Done(ReplDef(n, interp(e, env)))
      case Undef(n)   => Done(ReplUnDef(n))
      case Load(n)    => Done(ReplLoad(n))
      case ReLoad()   => Done(ReplReLoad())
      case Compile(e) => Done(Code(Compiler.compile(e)))
      case Run(e)     => Next(e, env)
      case Comment(_) => Done(MVoid)
      case c: Const   => Done(c)
    }
  }
}