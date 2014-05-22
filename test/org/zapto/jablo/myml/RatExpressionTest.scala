/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

import org.junit._
import Assert._
import Sculla.calc
import Sculla.e
import Sculla.check

class RatExpressionTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def ratTest1 = {
    val p3 = check(calc.parseAll(calc.expr, "2/3*x^2"))
    assertEquals(Mul(Q(2, 3), Pot(Var("x"), Z(2))), p3.get)
    assertEquals(Q(32, 3), p3.get.eval(Map("x" -> Z(4))));
  }

}
