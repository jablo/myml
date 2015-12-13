/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.zapto.jablo.myml

import scala.collection._
import Ex.{ Env, MutEnv }

/**
 * Bytecode machine and interpreter environment - ie. map from identifiers to values
 */
abstract class BCEnv {
  def get(n: String): Option[Const]
  def +(n: String, e: Const): BCEnv
  def -(n: String): BCEnv
  def ++(es: List[(String, Const)]): BCEnv
  def +=(p: (String, Const)): BCEnv
  def -=(n: String): BCEnv
  def clear: Unit
  def keys: Iterable[String]
}

/**
 * Empty environment
 */
case object BCNilEnv extends BCEnv {
  def get(s: String): Option[Const] = { print("Lookup "); print(s); println("=None"); None }
  def +(n: String, e: Const): BCEnv = BCImmutEnv(this, Map() + (n -> e))
  def -(n: String): BCEnv = throw new RuntimeException("Cant undefine from empty scope")
  def ++(es: List[(String, Const)]): BCEnv = BCImmutEnv(this, Map() ++ es)
  def +=(p: (String, Const)): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def -=(n: String): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = List()
}

/**
 * An environment - or store - with one or more variables defined; immutable version, used
 * for normal "let" and function call constructs
 */
case class BCImmutEnv(outer: BCEnv = BCNilEnv, val env: Env = Map()) extends BCEnv {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => { print("Lookup "); print(n); print("="); println(e); Some(e) }
    }
  }
  def +(n: String, e: Const): BCEnv = BCImmutEnv(outer, env + (n -> e))
  def -(n: String): BCEnv = BCImmutEnv(outer, env - n)
  def ++(es: List[(String, Const)]): BCEnv = BCImmutEnv(outer, env ++ es)
  def +=(p: (String, Const)): BCEnv = throw new RuntimeException("Not supported on immutable scopes")
  def -=(n: String): BCEnv = throw new RuntimeException("Not supported on immutable empty")
  def clear: Unit = throw new RuntimeException("Not supported on immutable empty")
  def keys = env.keys
}

/**
 * An environment - or store - with one or more variables defined, mutable version. The mutable
 * version is used for the "let*" construct to "tie the knots" so the variables being defined can do self or mutual references.
 */
case class BCMutEnv(outer: BCEnv, val env: MutEnv = mutable.Map()) extends BCEnv {
  def get(n: String): Option[Const] = {
    val v = env get n
    v match {
      case None    => outer get n
      case Some(e) => { print("Lookup "); print(n); print("="); println(e); Some(e) }
    }
  }
  def +(n: String, e: Const): BCEnv = { env += (n -> e); this }
  def -(n: String): BCEnv = { env -= n; this }
  def ++(es: List[(String, Const)]): BCEnv = { env ++= es; this }
  def ++=(es: List[(String, Const)]): BCEnv = { env ++= es; this } // Same as ++ - but more readable; for the direct interpreter
  def +=(p: (String, Const)): BCEnv = { env += (p._1 -> p._2); this }
  def -=(n: String): BCEnv = { env -= n; this }
  def clear: Unit = env.clear
  def keys = env.keys
}
