/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import org.junit._
import Assert._
import TestHelper.calc
import TestHelper.e
import TestHelper.check
import TestHelper.reparse

class RatExpressionTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def ratTest1 = {
    val p = check(calc.parseAll(calc.expr, "2/3*x^2"))
    assertEquals(Mul(Q(2, 3), Pot(Var("x"), Z(2))), p.get)
    assertEquals(Q(32, 3), p.get.eval(Map("x" -> Z(4))));
    assertEquals(p.get, reparse(p))
    assertEquals(Q(32,3), ByteCodeMachine.interp(p.get, Map("x"->Z(4))))
  }

  @Test
  def ratTest2 = {
    val p = check(calc.parseAll(calc.expr, "2/3+4/5+6/7+8/9"))
    println("ratTets2 - parse: "+ p.get)
    println("         - infix: "+ p.get.infix)
    val ev = p.get.eval(Map())
    println("         - ev   : "+ ev)
    println("         - evinf: "+ ev.infix)
  }


}
