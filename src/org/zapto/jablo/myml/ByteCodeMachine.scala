package org.zapto.jablo.myml

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection._
import Ex.{ Env, MutEnv }

abstract class BCEnv {
  def get(n: String): Option[Const]
  def +(n: String, e: Const): BCEnv
  def -(n: String): BCEnv
  def ++(es: List[(String, Const)]): BCEnv
  def +=(p: Pair[String, Const]): BCEnv
  def -=(n: String): BCEnv
  def clear: Unit
  def keys: Iterable[String]
}

case class BCNilEnv extends BCEnv {
  def get(s: String): Option[Const] = None
  def +(n: String, e: Const): BCEnv = BCImmutEnv(this, Map() + Pair(n, e))
  def -(n: String): BCEnv = throw new RuntimeException("Cant undefine from empty scope")
  def ++(es: List[(String, Const)]): BCEnv = BCImmutEnv(this, Map() ++ es)
  def +=(p: Pair[String,Const]): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def -=(n: String): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = List()
}

case class BCImmutEnv(outer: BCEnv = BCNilEnv(), val env: Env = Map()) extends BCEnv {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => Some(e)
    }
  }
  def +(n: String, e: Const): BCEnv = BCImmutEnv(outer, env + Pair(n, e))
  def -(n: String): BCEnv = BCImmutEnv(outer, env - n)
  def ++(es: List[(String, Const)]): BCEnv = BCImmutEnv(outer, env ++ es)
  def +=(p: Pair[String,Const]): BCEnv = throw new RuntimeException("Not supported on immutable scopes")
  def -=(n: String): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = env.keys
}

case class BCMutEnv(outer: BCEnv, val env: MutEnv = mutable.Map()) extends BCEnv {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => Some(e)
    }
  }
  def +(n: String, e: Const): BCEnv = { env += Pair(n, e); this }
  def -(n: String): BCEnv = { env -= n; this }
  def ++(es: List[(String, Const)]): BCEnv = { env ++= es; this }
  def +=(p: Pair[String,Const]): BCEnv = { env += Pair(p._1, p._2); this }
  def -=(n: String): BCEnv = { env -= n; this }
  def clear: Unit = env.clear
  def keys = env.keys
}

object ByteCodeMachine {
  final def interp(e: Ex, env: BCEnv = BCNilEnv()): Const = interp(Compiler.compile(e), env)
  final def interp(insns: List[ByteCode], bcenv: BCEnv): Const = {
    val stack = new Stack[Const]()
    //    val bcenv = BCEnv(NilScope(), benv)
    interp1(stack, insns, bcenv)
  }
  @tailrec
  final def interp1(stack: Stack[Const], insns: List[ByteCode], env: BCEnv): Const = {
    insns match {
      case Nil => if (!stack.isEmpty) stack.top else MVoid
      case insn :: insns1 =>
        println("Exec: " + insn)
        val (stack1, scope1, morecode) = insn.exec(stack, env)
        interp1(stack1, morecode ++ insns1, scope1)
    }
  }
}

abstract class ByteCode extends ExHelper {
  type MStack = Stack[Const]
  type Store = (MStack, BCEnv, List[ByteCode])
  def exec(stack: Stack[Const], env: BCEnv): Store;
  protected def pop(s: MStack): (Const, MStack) = (s.top, s.pop) // pop as it should have been. Sheesh.
  protected val none = List[ByteCode]()
}

case class Push(c: Const) extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = (stack.push(c), env, none)
}

case object Lookup extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = {
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
  def exec(stack: MStack, env: BCEnv): Store = {
    val (Subr(args, code, _), s1) = pop(stack)
    (s1.push(Subr(args, code, env)), env, none)
  }
}

case object Call extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = {
    val (subr, s1) = pop(stack)
    subr match {
      case Subr(fargs, code, fenv) =>
        val n = fargs size
        val argvalpairs = (fargs reverse) zip (s1 take n)
        val s2 = s1 drop n
        (s2, fenv ++ argvalpairs, code)
    }
  }
}

case object Cond extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = {
    val (test, s1) = pop(stack)
    val (Code(fcode), s2) = pop(s1)
    val (Code(tcode), s3) = pop(s2)
    (s3, env, if (test == True) tcode else fcode)
  }
}

case object RecEnv extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = (stack, BCMutEnv(env), none)
}

case object Assign extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = {
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
  def exec(stack: MStack, env: BCEnv): Store = {
    val (Str(n), s1) = pop(stack)
    (s1, env - n, none)
  }
}

case object ErrIns extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = throw new RuntimeException(stack.pop toString)
}

case object NotImplemented extends ByteCode {
  def exec(stack: MStack, env: BCEnv): Store = throw new RuntimeException("not implemented")
}
