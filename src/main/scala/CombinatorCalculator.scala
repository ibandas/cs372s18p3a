package edu.luc.cs.laufer.cs473.expressions

object CombinatorCalculator extends App {

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.expr, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("It has size " + size(expr) + " and height " + height(expr))
      println("It evaluates to " + evaluate(expr))
    }
  }

  def processStatement(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.statement, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val statement = result.get
      println("The parsed expression is: ")
      println(statement)
      println("The unparsed expression is: ")
      //println(toFormattedString(statement))
      //println("It has size " + size(statement) + " and height " + height(statement))
      //println("It evaluates to " + evaluate(statement))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processStatement(line)
      print("Enter infix expression: ")
    }
  }
}
