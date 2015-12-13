/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

/**
 * Byte code base class and shared definitions.
 */
abstract class ByteCode extends ExHelper {
  /**
   * Machine stack - a list of constant values
   */
  type MStack = List[Const]
  /**
   * Machine store - a stack, an environment of variables, and a list of instructions
   */
  type MachineState = (MStack, BCEnv, List[ByteCode])
  /**
   * Exceute a byte code instruction in a specific machine state, giving a new machine state
   */
  def exec(stack: List[Const], env: BCEnv): MachineState;
  protected val noCode = List[ByteCode]()
}

case class Push(c: Const) extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = (c :: stack, env, noCode)
}

case object Lookup extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case Str(n) :: s1 =>
        val v = env get n
        v match {
          case None    => err("Variable not found " + n)
          case Some(c) => (c :: s1, env, noCode)
        }
      case _ => typerr("Extected string", stack head)
    }
  }
}

case object MakeClosure extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case Subr(args, code, _) :: s1 => (Subr(args, code, env) :: s1, env, noCode)
      case _                         => typerr("Extected Subr", stack head)
    }
  }
}

case object Call extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case Subr(fargs, code, fenv) :: s1 =>
        val n = fargs size
        val argvalpairs = (fargs reverse) zip (s1 take n)
        val s2 = s1 drop n
        (s2, fenv ++ argvalpairs, code) // TODO: Is this a bug? Should it bw argvalpairs ++ fenv ?
      case _ => typerr("Extected Subr", stack head)
    }
  }
}

case object Cond extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case test :: Code(fcode) :: Code(tcode) :: s1 => (s1, env, if (test == True) tcode else fcode)
      case _                                        => typerr("Extected test :: code :: code", stack head)
    }
  }
}

case object RecEnv extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = (stack, BCMutEnv(env), noCode)
}

case object Assign extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case Str(n) :: v :: s2 => (s2, env + (n, v), noCode)
      case _                 => typerr("Expected string", stack head)
    }
  }
}

case object UnAssign extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = {
    stack match {
      case Str(n) :: s1 => (s1, env - n, noCode)
      case _            => typerr("Extected string", stack head)
    }
  }
}

case object ErrIns extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = throw new RuntimeException(stack.head toString)
}

case object NotImplemented extends ByteCode {
  def exec(stack: MStack, env: BCEnv): MachineState = throw new RuntimeException("not implemented")
}
