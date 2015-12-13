package org.zapto.jablo.myml

import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}

class SimpleExprSuite extends TestHelper {
  test("A simple constant expression should evaluate to itself") {
    val p = parser.parseAll(parser.expr, "1")
    assert(Z(1) == p.get);
    val ev = ByteCodeMachine.interp(Compiler.compile(p.get), e)
    assert(p.get == ev)
  }

  test("A binary addition should parse to an Add node, evauate to the sum of the terms, and the infix form should parse to itself") {
    val p = parser.parseAll(parser.expr, "1+2")
    assert(Bin.Add(Z(1), Z(2)) == p.get)
    val ev: Const = ByteCodeMachine.interp(Compiler.compile(p.get), e)
    assert((Z(3): Ex) == ev)
    assert(p.get == reparse(p))
  }

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
  test("Expression (6+2)*3") {
    val p = parser.parseAll(parser.expr, "(6+2)*3")
    assert(Bin.Mul(Par(Bin.Add(Z(6), Z(2))), Z(3)) == p.get)
    assert((Z(24):Ex) == eval(p.get, Map("x" -> Z(5))))
    assert(p.get == reparse(p))
  }

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
  //

  test("Expression 2*x*-x") {
    val p = parser.parseAll(parser.expr, "2*x*-x")
    assert(Bin.Mul(Bin.Mul(Z(2), Var("x")), Un.Neg(Var("x"))) == p.get)
    assert((Z(-50):Ex) == eval(p.get, Map("x" -> Z(5))))
    assert(p.get == reparse(p))
  }

}
