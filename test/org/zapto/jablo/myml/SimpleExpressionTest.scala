/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */

package org.zapto.jablo.myml

import org.junit._
import Assert._
import org.zapto.jablo.myml.TestHelper._
//import TestHelper.calc
//import TestHelper.e
//import TestHelper.check
//import TestHelper.reparse
import org.zapto.jablo.myml.Ex._
import ByteCodeMachine._
import Compiler._

class SimpleExpressionTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def simeplExpr1 = {
    val p = check(calc.parseAll(calc.expr, "1"))
    assertEquals(Z(1), p.get);
    val ev = ByteCodeMachine.interp(Compiler.compile(p.get), e)
    assertEquals(Z(1), ev)
  }

//  @Test
//  def simeplExpr2 = {
//    val p = check(calc.parseAll(calc.expr, "1+2"))
//    assertEquals(Add(Z(1), Z(2)), p.get);
//    assertEquals(Z(3), p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def simeplExpr3 = {
//    val p = check(calc.parseAll(calc.expr, "1-2"))
//    assertEquals(Sub(Z(1), Z(2)), p.get);
//    assertEquals(Z(-1), p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def simeplExpr4 = {
//    val p = check(calc.parseAll(calc.expr, "3*2"))
//    assertEquals(Mul(Z(3), Z(2)), p.get);
//    assertEquals(Z(6), p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def simeplExpr5 = {
//    val p = check(calc.parseAll(calc.expr, "(6*2)/3"))
//    assertEquals(Div(Par(Mul(Z(6), Z(2))), Z(3)), p.get);
//    assertEquals(Q(4, 1), p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def simeplExpr6 = {
//    val p = check(calc.parseAll(calc.expr, "(6+2)*3"))
//    assertEquals(Mul(Par(Add(Z(6), Z(2))), Z(3)), p.get);
//    assertEquals(Z(24), p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def negExpr1 = {
//    val p = check(calc.parseAll(calc.expr, "-x"))
//    assertEquals(Neg(Var("x")), p.get);
//    assertEquals(Z(-24), p.get.eval(Map("x" -> Z(24))))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def negExpr2 = {
//    val p = check(calc.parseAll(calc.expr, "3*(-x)"))
//    assertEquals(Mul(Z(3), Par(Neg(Var("x")))), p.get);
//    assertEquals(Z(12), p.get.eval(Map("x" -> Z(-4))))
//    assertEquals(p.get, reparse(p))
//  }
//
//  @Test
//  def negExpr3 = {
//    val p = check(calc.parseAll(calc.expr, "2*x*-x"))
//    assertEquals(Mul(Mul(Z(2), Var("x")), Neg(Var("x"))), p.get);
//    assertEquals(Z(-50), p.get.eval(Map("x" -> Z(5))))
//    assertEquals(p.get, reparse(p))
//  }
}
