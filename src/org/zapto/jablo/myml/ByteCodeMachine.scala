/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import Ex.{ Env, MutEnv, err, typerr }
import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection._

abstract class BCScope {
  def get(n: String): Option[Const]
  def +(n: String, e: Const): BCScope
  def -(n: String): BCScope
  def ++(es: List[(String, Const)]): BCScope
  def +=(p: Pair[String, Const]): BCScope
  def -=(n: String): BCScope
  def clear: Unit
  def keys: Iterable[String]
}

case class NilScope extends BCScope {
  def get(s: String): Option[Const] = None
  def +(n: String, e: Const): BCScope = BCEnv(this, Map() + Pair(n, e))
  def -(n: String): BCScope = throw new RuntimeException("Cant undefine from empty scope")
  def ++(es: List[(String, Const)]): BCScope = BCEnv(this, Map() ++ es)
  def +=(p: Pair[String,Const]): BCScope = throw new RuntimeException("Not supported on immutable empty")
  def -=(n: String): BCScope = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = List()
}

case class BCEnv(outer: BCScope = NilScope(), val env: Env = Map()) extends BCScope {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => Some(e)
    }
  }
  def +(n: String, e: Const): BCScope = BCEnv(outer, env + Pair(n, e))
  def -(n: String): BCScope = BCEnv(outer, env - n)
  def ++(es: List[(String, Const)]): BCScope = BCEnv(outer, env ++ es)
  def +=(p: Pair[String,Const]): BCScope = throw new RuntimeException("Not supported on immutable scopes")
  def -=(n: String): BCScope = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = env.keys
}

case class BCMutEnv(outer: BCScope, val env: MutEnv = mutable.Map()) extends BCScope {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => Some(e)
    }
  }
  def +(n: String, e: Const): BCScope = { env += Pair(n, e); this }
  def -(n: String): BCScope = { env -= n; this }
  def ++(es: List[(String, Const)]): BCScope = { env ++= es; this }
  def +=(p: Pair[String,Const]): BCScope = { env += Pair(p._1, p._2); this }
  def -=(n: String): BCScope = { env -= n; this }
  def clear: Unit = env.clear
  def keys = env.keys
}

class ByteCodeMachine {

}

object ByteCodeMachine {
  final def interp(e: Ex, env: BCScope = NilScope()): Const = interp(e.bytecode, env)
  final def interp(insns: List[ByteCode], bcenv: BCScope): Const = {
    val stack = new Stack[Const]()
    //    val bcenv = BCEnv(NilScope(), benv)
    interp1(stack, insns, bcenv)
  }
  @tailrec
  final def interp1(stack: Stack[Const], insns: List[ByteCode], env: BCScope): Const = {
    insns match {
      case Nil => if (!stack.isEmpty) stack.top else MVoid
      case insn :: insns1 =>
        println("Exec: " + insn)
        val (stack1, scope1, morecode) = insn.exec(stack, env)
        println("    Stack: " + (stack1 take 6) + "...")
        val newcode = morecode ++ insns1
        interp1(stack1, newcode, scope1)
    }
  }
}

abstract class ByteCode extends ExHelper {
  type MStack = Stack[Const]
  type Store = (MStack, BCScope, List[ByteCode])
  def exec(stack: Stack[Const], env: BCScope): Store;
  protected def pop(s: MStack): (Const, MStack) = (s.top, s.pop) // pop as it should have been. Sheesh.
  protected val none = List[ByteCode]()
}

case class Push(c: Const) extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = (stack.push(c), env, none)
}

case object Lookup extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (nc, s1) = pop(stack)
    val n = nc match {
      case Str(s) => s
      case _      => typerr("Extected string", nc)
    }
    val v = env get n
    v match {
      case None    => err("Variable not found " + n)
      case Some(c) => (s1.push(c), env, none)
    }
  }
}

case object MakeClosure extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (Subr(args, code, _), s1) = pop(stack)
    (s1.push(Subr(args, code, env)), env, none)
  }
}

case object Call extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (subr, s1) = pop(stack)
    subr match {
      case Subr(fargs, code, fenv) =>
        val n = fargs size
        val argvalpairs = (fargs reverse) zip (s1 take n)
        val s2 = s1 drop n
        (s2, fenv ++ argvalpairs, code)
      // Cheating a bit - retrofit a Clo(...) created through direct interpretation into a compiled bytecode function        
      case Clo(args, body, env1) =>
        val code = body.bytecode
        val n = args size
        val argvalpairs = (args reverse) zip (s1 take n)
        val s2 = s1 drop n
        (s2, BCEnv(NilScope(), env1 ++ argvalpairs), code)
    }
  }
}

case object Cond extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (test, s1) = pop(stack)
    val (Code(fcode), s2) = pop(s1)
    val (Code(tcode), s3) = pop(s2)
    (s3, env, if (test == True) tcode else fcode)
  }
}

case object RecEnv extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = (stack, BCMutEnv(env), none)
}

case object Assign extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (nc, s1) = pop(stack)
    val (v, s2) = pop(s1)
    val n = nc match {
      case Str(s) => s
      case _      => typerr("Expected string", nc)
    }
    (s2, env + (n, v), none)
  }
}

case object UnAssign extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (Str(n), s1) = pop(stack)
    (s1, env - n, none)
  }
}

case object ErrIns extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException(stack.pop toString)
}

case object NotImplemented extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented")
}
