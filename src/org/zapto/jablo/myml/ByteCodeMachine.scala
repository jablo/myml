/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import Ex.Env
import scala.annotation.tailrec
import scala.collection.immutable.Stack

abstract class BCScope {
  def get(n: String): Option[Const]
  def +(n: String, e: Const): BCScope
  def ++(es: List[(String, Const)]): BCScope
  def keys: Iterable[String]
}
case class NilScope extends BCScope {
  def get(s: String): Option[Const] = None
  def +(n: String, e: Const): BCScope = BCEnv(this, Map() + Pair(n, e))
  def ++(es: List[(String, Const)]): BCScope = BCEnv(this, Map() ++ es)
  def keys = List()
}
case class BCEnv(outer: BCScope, val env: Env = Map()) extends BCScope {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => Some(e)
    }
  }
  def +(n: String, e: Const): BCEnv = BCEnv(outer, env + Pair(n, e))
  def ++(es: List[(String, Const)]): BCEnv = BCEnv(outer, env ++ es)
  def keys = env.keys
}

class ByteCodeMachine {

}

object ByteCodeMachine {
  final def interp(insns: List[ByteCode], env: Env): Const = {
    val stack = new Stack[Const]()
    val bcenv = BCEnv(NilScope(), env)
    interp1(stack, insns, bcenv)
  }
  @tailrec
  final def interp1(stack: Stack[Const], insns: List[ByteCode], env: BCScope): Const = {
    insns match {
      case Nil => stack.top
      case insn :: insns1 =>
        val (stack1, scope1, morecode) = insn.exec(stack, env)
        val newcode = morecode ++ insns1
        interp1(stack1, newcode, scope1)
    }
  }
}

abstract class ByteCode extends ExHelper {
  type MStack = Stack[Const]
  type Store = (MStack, BCScope, List[ByteCode])
  def exec(stack: Stack[Const], env: BCScope): Store;
  protected def pop(s: MStack): (Const, MStack) = (s.top, s.pop)
  protected val none = List[ByteCode]()
}

case class Lookup(n: String) extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val v = env get n
    v match {
      case None    => err("Variable not found " + n)
      case Some(c) => (stack.push(c), env, none)
    }
  }
}

case class Push(c: Const) extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = (stack.push(c), env, none)
}

case class PushLexicalScope() extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = (stack, BCEnv(env), none)
}
case class PopLexicalScope() extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = (stack, env match {
    case BCEnv(outer, _) => outer
    case NilScope()      => err("No more stack frames to pop")
  },
    none)
}

case class Call extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (Subr(fargs, code), s1) = pop(stack)
    val n = fargs size
    val argvalpairs = (fargs reverse) zip (s1 take n)
    val s2 = s1 drop n
    (s2, env ++ argvalpairs, code)
  }
}

case class Cond extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (test, s1) = pop(stack)
    val (Code(tcode), s2) = pop(s1)
    val (Code(fcode), s3) = pop(s2)
    (s3, env, if (test == True) tcode else fcode)
  }
}

case class Assign(n: String) extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = {
    val (v, s1) = pop(stack)
    (s1, env + (n, v), none)
  }
}

case class SubStrIns extends ByteCode { def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented") }
case class TrimStrIns extends ByteCode { def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented") }
case class StrLenIns extends ByteCode { def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented") }
case class ToStrIns extends ByteCode { def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented") }

case class NotImplemented extends ByteCode { def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException("not implemented") }

case class ErrIns extends ByteCode {
  def exec(stack: MStack, env: BCScope): Store = throw new RuntimeException(stack.pop toString)
}