/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import org.junit._
import Assert._
import Sculla.calc
import Sculla.e
import Sculla.check
import Sculla.reparse

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
    assertEquals(p.get, reparse(p))
  }

  @Test
  def cmpExpr2 = {
    val p = check(calc.parseAll(calc.expr, "1 > 2"))
    assertEquals(Gt(Z(1), Z(2)), p.get);
    assertEquals(False, p.get.eval(e))
    assertEquals(p.get, reparse(p))
  }

  @Test
  def cmpExprLte = {
    val p = check(calc.parseAll(calc.expr, "1 <= 2"))
    assertEquals(Lte(Z(1), Z(2)), p.get);
    assertEquals(True, p.get.eval(e))
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprRat0 = {
    val p = check(calc.parseAll(calc.expr, "2/3 = 4/6"))
    assertEquals(Equ(Q(2, 3), Q(4, 6)), p.get)
    assertEquals(True, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprRat0a = {
    val p = check(calc.parseAll(calc.expr, "2/3 <> 4/6"))
    assertEquals(Neq(Q(2, 3), Q(4, 6)), p.get)
    assertEquals(False, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(False, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprRat1 = {
    val p = check(calc.parseAll(calc.expr, "2/3 <= 4/5"))
    assertEquals(Lte(Q(2, 3), Q(4, 5)), p.get)
    assertEquals(True, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprRat2 = {
    val p = check(calc.parseAll(calc.expr, "0/3 <= 0/5"))
    assertEquals(Lte(Q(0, 3), Q(0, 5)), p.get)
    assertEquals(True, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprZRat1 = {
    val p = check(calc.parseAll(calc.expr, "2 = 8/4"))
    assertEquals(Equ(Z(2), Q(8, 4)), p.get)
    assertEquals(True, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }

  @Test
  def cmpExprZRat2 = {
    val p = check(calc.parseAll(calc.expr, "3/2 <= 8"))
    assertEquals(Lte(Q(3, 2), Z(8)), p.get)
    assertEquals(True, p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(True, ByteCodeMachine.interp(p.get))
  }
}
