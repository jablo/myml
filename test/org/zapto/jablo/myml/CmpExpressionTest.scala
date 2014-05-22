/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

import org.junit._
import Assert._
import Sculla.calc
import Sculla.e
import Sculla.check

class CmpExpressionTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def cmpExpr1 = {
    val p = check(calc.parseAll(calc.expr, "1 < 2"))
    assertEquals(Lt(Z(1), Z(2)), p.get);
    assertEquals(True, p.get.eval(e))
  }

  @Test
  def cmpExpr2 = {
    val p = check(calc.parseAll(calc.expr, "1 > 2"))
    assertEquals(Gt(Z(1), Z(2)), p.get);
    assertEquals(False, p.get.eval(e))
  }
  
  @Test
  def cmpExprLte = {
    val p = check(calc.parseAll(calc.expr, "1 <= 2"))
    assertEquals(Lte(Z(1), Z(2)), p.get);
    assertEquals(True, p.get.eval(e))
  }
  
  
}
