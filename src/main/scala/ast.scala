package edu.luc.cs.laufer.cs473.expressions.ast

import scala.util.parsing.input.Positional

/** An initial algebra of arithmetic expressions. */
//Stuff similar to misc-scala
sealed trait Expr
case class Constant(value: Int) extends Expr

/** A binary statement with two non-null children. */
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr {
  require { (left != null) && (right != null) }
}

case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)

/**Imperative statements, that is, those that are interesting because of their side effects. */
case class Variable(name: String) extends Expr {
  require(name != null)
}

case class Sequence(statements: Expr*) extends Expr {
  require(statements != null)
  require(!statements.contains(null))
}

case class While(guard: Expr, body: Expr) extends BinaryExpr(guard, body)
case class Assignment(left: Expr, right: Expr) extends BinaryExpr(left, right)

//Stuff from original expressions
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)

//My Edits
sealed trait WorkflowToken extends Positional

case class identifier(str: String) extends WorkflowToken
case class literal(str: String) extends WorkflowToken
case class indentation(spaces: Int) extends WorkflowToken
case class exit() extends WorkflowToken
case class readInput() extends WorkflowToken
case class callService() extends WorkflowToken
case class switch() extends WorkflowToken
case class otherwise() extends WorkflowToken
case class colon() extends WorkflowToken
case class arrow() extends WorkflowToken
case class equals() extends WorkflowToken
case class comma() extends WorkflowToken
case class indent() extends WorkflowToken
case class dedent() extends WorkflowToken

