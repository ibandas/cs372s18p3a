package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedStatementExpr = CombinatorParser.parseAll(CombinatorParser.statement, complex1string)
  println(parsedStatementExpr.get)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedStatementExpr = CombinatorParser.parseAll(CombinatorParser.statement, binaryConditionalString)
  val parsedStatementExpr2 = CombinatorParser.parseAll(CombinatorParser.statement, assignmentString)
  val parsedStatementExpr3 = CombinatorParser.parseAll(CombinatorParser.statement, simpleString)
  val parsedStatementExpr4 = CombinatorParser.parseAll(CombinatorParser.statement, blockString)
  val parsedStatementExpr5 = CombinatorParser.parseAll(CombinatorParser.statement, complexAssignmentString)
  val parsedStatementExpr6 = CombinatorParser.parseAll(CombinatorParser.statement, complexWhileString)
  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
  test("binary conditional parser works 1") { assert(parsedStatementExpr.get === binaryConditionalStatement) }
  test("assignment parser works 2") { assert(parsedStatementExpr2.get === assignmentStatement) }
  test("simple statement parser works 3") { assert(parsedStatementExpr3.get === simpleStatement) }
  test("block statement parser works 4") { assert(parsedStatementExpr4.get === blockStatement) }
  test("complex assignment statement parser works 5") { assert(parsedStatementExpr5.get === complexAssignmentStatement) }
  test("complex while statement parser works 6") { assert(parsedStatementExpr6.get === complexWhileStatement) }
}
