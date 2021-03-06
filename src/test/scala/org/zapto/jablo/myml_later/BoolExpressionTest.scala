///*
// * (C) 2014 Jacob Lorensen, jacoblorensen@gmail.com
// */
//
//package org.zapto.jablo.myml
//
//import org.junit._
//import Assert._
//import TestHelper.calc
//import TestHelper.e
//import TestHelper.check
//import TestHelper.reparse
//
//class BoolExpressionTest {
//  @Before
//  def setUp: Unit = {
//  }
//
//  @After
//  def tearDown: Unit = {
//  }
//
//  @Test
//  def boolTrue = {
//    val p = check(calc.parseAll(calc.expr, "true"))
//    assertEquals(True, p.get);
//    assertEquals(True, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(True, ByteCodeMachine.interp(p.get))
//
//    val p2 = check(calc.parseAll(calc.expr, "false"))
//    assertEquals(False, p2.get);
//    assertEquals(False, p2.get.eval(e))
//    assertEquals(p2.get, reparse(p2))
//    assertEquals(False, ByteCodeMachine.interp(p2.get))
//
//  }
//
//  @Test
//  def boolTrueAndTrue = {
//    val p = check(calc.parseAll(calc.expr, "true and true"))
//    assertEquals(And(True, True), p.get);
//    assertEquals(True, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(True, ByteCodeMachine.interp(p.get))
//  }
//
//  @Test
//  def boolTrueAndFalse = {
//    val p = check(calc.parseAll(calc.expr, "true and false"))
//    assertEquals(And(True, False), p.get);
//    assertEquals(False, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(False, ByteCodeMachine.interp(p.get))
//
//  }
//
//  @Test
//  def boolFalseOrTrueAndTrue = {
//    val p = check(calc.parseAll(calc.expr, "false or true and true"))
//    assertEquals(Or(False, And(True, True)), p.get);
//    assertEquals(True, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(True, ByteCodeMachine.interp(p.get))
//
//  }
//
//  @Test
//  def boolNot1 = {
//    val p = check(calc.parseAll(calc.expr, "not false"))
//    assertEquals(Not(False), p.get);
//    assertEquals(True, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(True, ByteCodeMachine.interp(p.get))
//
//  }
//
//  @Test
//  def boolNot2 = {
//    val p = check(calc.parseAll(calc.expr, "not false and true"))
//    assertEquals(And(Not(False), True), p.get);
//    assertEquals(True, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(True, ByteCodeMachine.interp(p.get))
//
//  }
//
//  @Test
//  def boolNot3 = {
//    val p = check(calc.parseAll(calc.expr, "not (true or false)"))
//    assertEquals(Not(Par(Or(True, False))), p.get);
//    assertEquals(False, p.get.eval(e))
//    assertEquals(p.get, reparse(p))
//    assertEquals(False, ByteCodeMachine.interp(p.get))
//
//  }
//}
