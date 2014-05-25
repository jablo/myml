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

class IfExprTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def ifInt1: Unit = {
    try {
      val p = check(calc.parseAll(calc.expr, "if 1 then 2 else 0"))
      p.get eval e
      fail("Shuold produce TypeErrorException")
    } catch {
      case e:TypeErrorException => Unit
    }
  }

  
  @Test
  def ifLte1= {
    val p = check(calc.parseAll(calc.expr, "if x <= 4 then 6 else 7"))
    assertEquals(Ife(Lte(Var("x"), Z(4)), Z(6), Z(7)), p.get);
    assertEquals(Z(7), p.get.eval(Map("x" -> Z(12))))
      assertEquals(p.get, reparse(p))
}

  @Test
  def ifLteComposed = {
    val p = check(calc.parseAll(calc.expr, "if x+10 <= 4*x then 1 else 2"))
    assertEquals(Ife(Lte(Add(Var("x"), Z(10)), Mul(Z(4), Var("x"))), Z(1), Z(2)), p.get)
    assertEquals(Z(1), p.get.eval(Map("x" -> Z(4))))
    assertEquals(p.get, reparse(p))
  }

  
  @Test
  def ifBool1 = {
    val p = check(calc.parseAll(calc.expr, "if true then 2 else 0"))
    assertEquals(Ife(True, Z(2), Z(0)), p.get)
    assertEquals(Z(2), p.get eval e)
      assertEquals(p.get, reparse(p))
}

  @Test
  def ifNested1 = {
    val p = check(calc.parseAll(calc.expr, "if false then if true then 3 else 2 else 1"))
    assertEquals(Ife(False, Ife(True, Z(3), Z(2)), Z(1)), p.get)
    assertEquals(Z(1), p.get eval e)
      assertEquals(p.get, reparse(p))
}
  
  @Test 
  def ifHigher1 = {
    val p = check(calc.parseAll(calc.expr, "(if true then fun(x)=>x+1 else fun(x)=>2+x)(5)"))
    assertEquals(App(Par(Ife(True,Fun(List("x"), Add(Var("x"),Z(1))), Fun(List("x"), Add(Z(2),Var("x"))))),List(Z(5))), p.get)
    assertEquals(Z(6), p.get eval e)
      assertEquals(p.get, reparse(p))
}
}
