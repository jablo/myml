/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import scala.collection._
import org.zapto.jablo.myml.Ex._
import scala.annotation.tailrec

object Interpreter extends ExHelper {
//  @tailrec can't use it - there are both tailrec call (in App(...)) and non-tailrec calls (everywhere else)
  final def interp(e: Ex, env: Env): Const = {
    e match {
      case ErrorEx(n) => throw new MyMLException("Error: " + n)
      case Var(n) => env get n match {
        case None    => err("Unknown variable", e)
        case Some(v) => interp(v, env)
      }
      case Par(e1)         => interp(e1, env)
      case Un(e1, op)      => op.eval(interp(e1, env))
      case Bin(e1, e2, op) => op.eval(interp(e1, env), interp(e2, env))
      case Ife(e1, e2, e3) => {
        val test = interp(e1, env)
        test match {
          case True  => interp(e2, env)
          case False => interp(e3, env)
          case _     => typerr("boolean", e1)
        }
      }
      case Fun(fargs, body) => Clo(fargs, body, env)
      case App(fexp, args) => {
        val fun = interp(fexp, env)
        fun match {
          case _@ Clo(fargs, body, fenv) =>
            val actarg = args map ((arg: Ex) => interp(arg, env))
            val env2 = fenv ++ Map(fargs zip actarg: _*)
            interp(body, env2)
          case _ => typerr("Not a function", fexp)
        }
      }
      case LetR(fargs, args, body) => {
        // Use a mutable map initialized with current env so we can evaluate arguments in their own environmnent, creating a cyclic environment
        val letrecenv = mutable.Map[String, Const](env toList: _*)
        val actarg = args map (interp(_, letrecenv));
        letrecenv ++= fargs zip actarg
        interp(body, letrecenv)
      }
      case Def(n, e)  => ReplDef(n, interp(e, env))
      case Undef(n)   => ReplUnDef(n)
      case Load(n)    => ReplLoad(n)
      case ReLoad()   => ReplReLoad()
      case Compile(e) => Code(Compiler.compile(e))
      case Comment(_) => MVoid
      case c: Const   => c
    }
  }
}