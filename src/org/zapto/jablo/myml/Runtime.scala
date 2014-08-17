/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import scala.language.implicitConversions

trait ExHelper {
  final implicit def booleanToConst(b: Boolean): Const = if (b) True else False
  final implicit def bigintToConst(b: BigInt): Const = Z(b)
  final implicit def stringToConst(s: String): Const = Str(s)
//  final implicit def constToEvalStep(c: Const): EvalStep = Done(c)

  final def typerr(s: String, e: Ex): Nothing = throw new TypeErrorException(s + " in: " + e.infix)
  final def err(s: String, e: Ex): Nothing = throw new MyMLException(s + " " + e.infix)
  final def err(s: String): Nothing = throw new MyMLException(s)
}

class MyMLException(msg: String = null, cause: Throwable = null) extends java.lang.Exception(msg, cause)
class TypeErrorException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause)
class UndefinedOperationException(msg: String = null, cause: Throwable = null) extends MyMLException(msg, cause)

