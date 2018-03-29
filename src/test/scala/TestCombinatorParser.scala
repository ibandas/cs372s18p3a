package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedStatementExpr = CombinatorParser.parseAll(CombinatorParser.statement, complexStatement)
  println(parsedStatementExpr.get)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedStatementExpr = CombinatorParser.parseAll(CombinatorParser.statement, complexStatement)
  val parsedStatementExpr2 = CombinatorParser.parseAll(CombinatorParser.statement, simpleStatement)
  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
  test("statement parser works 1") { assert(parsedStatementExpr.get === complex1Statement) }
  test("statement parser works 2") { assert(parsedStatementExpr2.get === simple1Statement) }
}
