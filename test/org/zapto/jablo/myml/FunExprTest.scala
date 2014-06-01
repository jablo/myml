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

class FunExprTest {
  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def funTest1 = {
    println("funTest1")
    val p = check(calc.parseAll(calc.expr, "fun (x) => 2*x"))
    assertTrue(p.successful)
    assertEquals(Fun(List("x"), Mul(Z(2), Var("x"))), p.get)
    val e = Map("x" -> Z(5))
    assertEquals(Clo(List("x"), Mul(Z(2), Var("x")), e), p.get.eval(e))
    assertEquals(p.get, reparse(p))
    println("program: " + p.get.bytecode)
    val bcev = ByteCodeMachine.interp(p.get)
    println("funTest1 - bcev: " + bcev)
    bcev match {
      case Subr(args, code, _) => 
        assertEquals(args, List("x"))
        assertEquals(code, List(Push(Z(2)), Push(Str("x")), Lookup, OMul))
      case _ => fail("Woups")
    }
    
  }

  @Test
  def letTest1 = {
    val p = calc.parseAll(calc.expr, "let a = 1; b = 2 in a+b")
    check(p)
    assertEquals(App(Fun(List("a", "b"), Add(Var("a"), Var("b"))), List(Z(1), Z(2))), p.get)
    assertEquals(Z(3), p.get.eval(Map()))
    assertEquals(Z(3), p.get.eval(Map("a" -> Z(45))))
    println("letTest1 - pars: " + p.get)
    println("         - infix: " + p.get.infix)
    val rp = reparse(p)
    println("         - repar: " + rp)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(3), ByteCodeMachine.interp(p.get))
    assertEquals(Z(3), ByteCodeMachine.interp(p.get, Map("x" -> Z(4))))
  }

  @Test
  def funTest2 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 5*x+2)")
    check(p)
    val body = Add(Mul(Z(5), Var("x")), Z(2))
    val pexp = Par(Fun(List("x"), body))
    assertEquals(pexp, p.get)
    assertEquals(Clo(List("x"), body, Map()), p.get.eval(Map()))
    assertEquals(p.get, reparse(p))
    assertEquals(Subr(List("x"), List(Push(Z(5)),Push(Str("x")),Lookup,OMul,Push(Z(2)),OAdd), BCEnv(NilScope(), e)), ByteCodeMachine.interp(p.get))
  }

  @Test
  def appTest2 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 3*x+1) (4)")
    check(p)
    val fexp = Fun(List("x"), Add(Mul(Z(3), Var("x")), Z(1)))
    val exp = App(fexp, List(Z(4)))
    assertEquals(exp, p.get)
    assertEquals(Z(13), p.get.eval(Map()))
    assertEquals(p.get, reparse(p))
    assertEquals(Z(13), ByteCodeMachine.interp(p.get))
  }

  @Test
  def appTest3 = {
    val p = calc.parseAll(calc.expr, "(fun (x) => 7*x+7 )(4)")
    check(p)
    val body = Add(Mul(Z(7), Var("x")), Z(7))
    val fexp = Fun(List("x"), body)
    val app = App(fexp, List(Z(4)))
    assertEquals(app, p.get)
    assertEquals(Z(35), p.get.eval(Map()))
    assertEquals(p.get, reparse(p))
    assertEquals(Z(35), ByteCodeMachine.interp(p.get))
  }

  @Test
  def letFunTest1 = {
    val p = calc.parseAll(calc.expr, "let f=fun (x) => 8*x+4 in f(4)")
    check(p)
    val exp = App(Fun(List("f"), App(Var("f"), List(Z(4)))), List(Fun(List("x"), Add(Mul(Z(8), Var("x")), Z(4)))))
    assertEquals(exp, p.get)
    assertEquals(Z(36), p.get.eval(Map()))
    println("letFunTest1 infix: " + p.get.infix)
    val rp = reparse(p)
    println("letFunTest1 rpars: " + rp.infix)
    assertEquals(p.get, rp)
    assertEquals(Z(36), ByteCodeMachine.interp(p.get))
  }

  @Test
  def letFunTest1p = {
    val p = calc.parseAll(calc.expr, "let f=(fun (x) => 8*x+4) in f(4)")
    check(p)
    val exp = App(Fun(List("f"), App(Var("f"), List(Z(4)))), List(Par(Fun(List("x"), Add(Mul(Z(8), Var("x")), Z(4))))))
    assertEquals(exp, p.get)
    assertEquals(Z(36), p.get.eval(Map()))
    assertEquals(p.get, reparse(p))
    assertEquals(Z(36), ByteCodeMachine.interp(p.get))
  }

  @Test
  def letRecTest1 = {
    val p = calc.parseAll(calc.expr, "let* f=fun (x) => x*x in f(4)")
    check(p)
    val exp = LetR(List("f"), List(Fun(List("x"), Mul(Var("x"), Var("x")))), App(Var("f"), List(Z(4))))
    assertEquals(exp, p.get)
    assertEquals(Z(16), p.get eval Map())
    assertEquals(Z(16), p.get eval Map("x" -> Z(12)))
    println("letRecTest1 - pars: " + p.get)
    println("            - infix: " + p.get.infix)
    val rp = reparse(p)
    println("            - repar: " + rp)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(16), ByteCodeMachine.interp(p.get))
    assertEquals(Z(16), ByteCodeMachine.interp(p.get, Map("x"->Z(12))))
  }

  @Test
  def letRecTest4 = {
    val p = calc.parseAll(calc.expr, "let* f = fun(x) => if x=0 then 1 else x*f(x-1) in f(0)")
    check(p)
    assertEquals(Z(1), p.get eval Map())
    assertEquals(p.get, reparse(p))
    assertEquals(Z(1), ByteCodeMachine.interp(p.get))
  }

  @Test
  def dirAppTest1 = {
    val p = calc.parseAll(calc.expr, "(fun(x) => 2*x^2+3)(4)")
    check(p)
    assertEquals(Z(35), p.get eval Map())
    assertEquals(p.get, reparse(p))
    assertEquals(Z(35), ByteCodeMachine.interp(p.get))
  }

  @Test
  def simpleLetRecursion = {
    val p = check(calc.parseAll(calc.expr, "let fac=fun(n,f)=>if n=0 then 1 else n*f(n-1,f) in fac(4,fac)"))
    assertEquals(Z(24), p.get eval e)
    println(p.get infix)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(24), ByteCodeMachine.interp(p.get))
  }

  @Test
  def simpleFunRecursion = {
    val p = check(calc.parseAll(calc.expr, "(fun(fac)=>fac(4, fac))(fun (n,f) => if n=0 then 1 else n*f(n-1, f))"))
    assertEquals(Z(24), p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(24), ByteCodeMachine.interp(p.get))
  }

  @Test
  def simpleFunRecursion2 = {
    val p = check(calc.parseAll(calc.expr, "let fak=fun(n)=>(fun(fac)=>fac(n, fac))(fun (n,f) => if n=0 then 1 else n*f(n-1, f)) in fak(5)"))
    assertEquals(Z(120), p.get eval e)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(120), ByteCodeMachine.interp(p.get))
  }

  @Test
  def yComb = {
    val p = check(calc.parseAll(calc.expr,
      "let Y  = fun(fu) => (fun(recur) => recur(recur)) (fun(recur) => fun(x) => fu(recur(recur))(x)); " +
        "    fac= fun(recur)=>fun(n)=>if n=0 then 1 else n*recur(n-1) in Y(fac)(5)"))
    assertEquals(Z(120), p.get eval e)
    println("yComb - pars: " + p.get)
    println("      - infix: " + p.get.infix)
    val rp = reparse(p)
    println("      - repar: " + rp)
    assertEquals(p.get, reparse(p))
    assertEquals(Z(120), ByteCodeMachine.interp(p.get))
  }
}