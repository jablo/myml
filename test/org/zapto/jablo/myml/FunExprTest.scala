/*
 * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
 */


package org.zapto.jablo.myml

import org.junit._
import Assert._
import Sculla.calc
import Sculla.e
import Sculla.check

class FunExprTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def funTest1 = {
    val p = check(calc.parseAll(calc.expr, "fun (x) => 2*x"))    
    assertTrue(p.successful)
    assertEquals(Fun(List("x"), Mul(Z(2), Var("x"))), p.get)
    val e = Map("x" -> Z(5))
    assertEquals(Clo(Fun(List("x"), Mul(Z(2), Var("x"))), e), p.get.eval(e))
  }

  @Test
  def letTest1 = {
    val p = calc.parseAll(calc.expr, "let a = 1; b = 2 in a+b")
    check(p)
    assertEquals(App(Fun(List("a", "b"), Add(Var("a"), Var("b"))), List(Z(1), Z(2))), p.get)
    assertEquals(Z(3), p.get.eval(Map()))
    assertEquals(Z(3), p.get.eval(Map("a" -> Z(45))))
  }

  @Test
  def funTest2 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 5*x+2)")
    check(p)
    val exp = Fun(List("x"), Add(Mul(Z(5), Var("x")), Z(2)))
    val pexp = Par(exp)
    assertEquals(pexp, p.get)
    assertEquals(Clo(exp, Map()), p.get.eval(Map()))
  }

  @Test
  def appTest2 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 3*x+1) (4)")
    check(p)
    val fexp = Fun(List("x"), Add(Mul(Z(3), Var("x")), Z(1)))
    val exp = App(Par(fexp), List(Z(4)))
    assertEquals(exp, p.get)
    assertEquals(Z(13), p.get.eval(Map()))
  }

  @Test
  def appTest3 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 7*x+7 )(4)")
    check(p)
    val body = Add(Mul(Z(7), Var("x")), Z(7))
    val fexp = Par(Fun(List("x"), body))
    val app = App(fexp, List(Z(4)))
    assertEquals(app, p.get)
    assertEquals(Z(35), p.get.eval(Map()))
  }

  @Test
  def letFunTest1 = {
    val p = calc.parseAll(calc.expr, "let f=fun (x) => 8*x+4 in f(4)")
    check(p)
    val exp = App(Fun(List("f"), App(Var("f"), List(Z(4)))), List(Fun(List("x"), Add(Mul(Z(8), Var("x")), Z(4)))))
    assertEquals(exp, p.get)
    assertEquals(Z(36), p.get.eval(Map()))
  }

  @Test
  def letFunTest1p = {
    val p = calc.parseAll(calc.expr, "let f=(fun (x) => 8*x+4) in f(4)")
    check(p)
    val exp = App(Fun(List("f"), App(Var("f"), List(Z(4)))), List(Par(Fun(List("x"), Add(Mul(Z(8), Var("x")), Z(4))))))
    assertEquals(exp, p.get)
    assertEquals(Z(36), p.get.eval(Map()))
  }

  @Test
  def letRecTest1 = {
    val p = calc.parseAll(calc.expr, "let* f=fun (x) => x*x in f(4)")
    check(p)
    val exp = LetR(List("f"), List(Fun(List("x"), Mul(Var("x"), Var("x")))), App(Var("f"), List(Z(4))))
    assertEquals(exp, p.get)
    assertEquals(Z(16), p.get eval Map())
    assertEquals(Z(16), p.get eval Map("x" -> Z(12)))
  }

  @Test
  def letRecTest2 = {
    val p = calc.parseAll(calc.expr, "let* f=fun (x) => if x then x*(f(x-1)) else 1 in f(4)")
    check(p)
    val exp = LetR(List("f"), List(Fun(List("x"), Ife(Var("x"), Mul(Var("x"), Par(App(Var("f"), List(Sub(Var("x"), Z(1)))))), Z(1)))), App(Var("f"), List(Z(4))))
    assertEquals(exp, p.get)
    assertEquals(Z(24), p.get eval Map())
    assertEquals(Z(24), p.get eval Map("x" -> Z(6), "y" -> Z(12)))
  }

  @Test
  def letRecTest3 = {
    val p = calc.parseAll(calc.expr, "let* f=fun (x) => if x then x*f(x-1) else 1 in f(4)")
    check(p)
    val exp = LetR(List("f"), List(Fun(List("x"), Ife(Var("x"), Mul(Var("x"), App(Var("f"), List(Sub(Var("x"), Z(1))))), Z(1)))), App(Var("f"), List(Z(4))))
    assertEquals(exp, p.get)
    assertEquals(Z(24), p.get eval Map())
    assertEquals(Z(24), p.get eval Map("x" -> Z(6), "y" -> Z(12)))
  }

  
  @Test
  def letRecTest4 = {
    val p = calc.parseAll(calc.expr, "let* f = fun(x) => if x=0 then 1 else x*f(x-1) in f(0)")
    check(p)
    println("And p is: " + p.get)
    assertEquals(Z(1), p.get eval Map())
  }

  
}
