package org.zapto.jablo.myml

import scala.language.postfixOps
import scala.annotation.tailrec
import scala.collection._

/**
 * The byte code machine combines a stack, a program (list of instructions) and a store (environment) of variables.
 */
object ByteCodeMachine {
  /**
   * Interpret an expression by first compiling the expression to byte code and then interpreting the byte code
   */
  final def interp(e: Ex, env: BCEnv = BCNilEnv): Const = interp(Compiler.compile(e), env)
  final def interp(insns: List[ByteCode]): Const = interp(insns, BCNilEnv)

  /**
   * Interpret a program in an environment - convenience function that allocates an empty stack first.
   */
  final def interp(insns: List[ByteCode], bcenv: BCEnv): Const = {
    val stack = List[Const]()
    interp1(stack, insns, bcenv)
  }

  /**
   * The byte code interpretation function
   */
  @tailrec
  final def interp1(stack: List[Const], insns: List[ByteCode], env: BCEnv): Const = {
    insns match {
      case Nil => if (!stack.isEmpty) stack.head else MVoid
      case insn :: insns1 =>
        //println("Exec: " + insn)
        val (stack1, scope1, morecode) = insn.exec(stack, env)
        interp1(stack1, morecode ++ insns1, scope1)
    }
  }
}

