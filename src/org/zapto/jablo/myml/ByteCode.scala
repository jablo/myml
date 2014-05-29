/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import Ex.Env
import scala.collection.immutable.Stack

abstract class ByteCode {
  type MStack = Stack[Const]
  type Store = (MStack, Env, List[ByteCode])
  def exec(stack: Stack[Const], env: Env): Store;
  protected def pop(s: MStack): (Const, MStack) = (s.top, s.pop)
  protected val none = List[ByteCode]()
}

case class Lookup(n: String) extends ByteCode {
  def exec(stack: MStack, env: Env): Store = (stack.push(env(n)), env, none)
}

case class Push(c: Const) extends ByteCode {
  def exec(stack: MStack, env: Env): Store = (stack.push(c), env, none)
}

case class Cond extends ByteCode {
  def exec(stack: MStack, env: Env): Store = {
    val (test, s1) = pop(stack)
    val (tr, s2) = pop(s1)
    val (Clo(_, body, env2), s3) = pop(s2)
    (s3, env2, body.compiled)
  }
}

case class Assign(n: String) extends ByteCode {
  def exec(stack: MStack, env: Env): Store = {
    val (v, s1) = pop(stack)
    (s1, env + Pair(n, v), none)
  }
}

case class Call extends ByteCode {
  def exec(stack: MStack, env: Env): Store = {
    val (Clo(expr, env, _), s1) = pop(stack)
    throw new RuntimeException("Not implemented yet")
  }
}

case class SubStrIns extends ByteCode { def exec(stack: Stack[Const], env: Env): Store = throw new RuntimeException("not implemented") }
case class TrimStrIns extends ByteCode { def exec(stack: Stack[Const], env: Env): Store = throw new RuntimeException("not implemented") }
case class StrLenIns extends ByteCode { def exec(stack: Stack[Const], env: Env): Store = throw new RuntimeException("not implemented") }
case class ToStrIns extends ByteCode { def exec(stack: Stack[Const], env: Env): Store = throw new RuntimeException("not implemented") }

case class NotImplemented extends ByteCode { def exec(stack: Stack[Const], env: Env): Store = throw new RuntimeException("not implemented") }

