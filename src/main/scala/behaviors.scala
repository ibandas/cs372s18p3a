package edu.luc.cs.laufer.cs473.expressions

import ast._
import scala.collection.immutable.Map

import scala.util.parsing.combinator.RegexParsers

/*
object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f]+".r

  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(exit | readInput | callService | switch | otherwise | colon | arrow | equals | comma | literal
      | identifier | indentation)) ^^ { rawTokens => processIndentations(rawTokens) }
  }

  private def processIndentations(tokens: List[WorkflowToken], indents: List[Int] = List(0)): List[WorkflowToken] = {
    tokens.headOption match {

      //Increase in indentation level, push into stack
      case Some(indentation(spaces)) if spaces > indents.head =>
        indent() :: processIndentations(tokens.tail, spaces :: indents)

      //Decrease in indentation level, pop from stack (dedent)
      case Some(indentation(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => dedent())) ::: processIndentations(tokens.tail, kept)

      //Indentation level unchanged, no tokens
      case Some(indentation(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      //Others tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      //Produce final dedents for levels remaining
      case None =>
        indents.filter(_ > 0).map(_ => dedent())
    }
  }
  def identifier: Parser[identifier] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => ast.identifier(str) }
  }

  def literal: Parser[ast.literal] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      ast.literal(content)
    }
  }

  def indentation: Parser[ast.indentation] = positioned {
    "\n[ ]*".r ^^ { whiteSpace =>
      val nSpaces = whiteSpace.length - 1
      ast.indentation(nSpaces)
    }
  }

  def exit = positioned { "exit" ^^ (_ => ast.exit()) }
  def readInput = positioned { "read input" ^^ (_ => ast.readInput()) }
  def callService = positioned { "call service" ^^ (_ => ast.callService()) }
  def switch = positioned { "switch" ^^ (_ => ast.switch()) }
  def otherwise = positioned { "otherwise" ^^ (_ => ast.otherwise()) }
  def colon = positioned { ":" ^^ (_ => ast.colon()) }
  def arrow = positioned { "->" ^^ (_ => ast.arrow()) }
  def equals = positioned { "==" ^^ (_ => ast.equals()) }
  def comma = positioned { "," ^^ (_ => ast.comma()) }
}
*/

//Right-hand side of an assignment
trait RValue[T] {
  def get: T
}

//Left-hand side of an assignment
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

//Cell for storing a value
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value; this }
}

object Cell {
  val NULL = Cell(0)
}

object behaviors {

  /*type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Expr): LValue[Int] = s match {
    case Constant(value)    => Cell(value)
    case Plus(left, right)  => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right)   => Cell(apply(store)(left).get / apply(store)(right).get)
    case Variable(name)     => store(name)
    case Assignment(left, right) => {
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
  }*/

  //TODO: case block => S.map(Expr)
  //Above: Because it's a sequence of statements for block, you want to make different cases for it to go into
  //whatever expression that it is.
  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
    //case Assignment(l, r) => Variable(l) = r
    //case Sequence(l)      => evaluate(l)
    //case While(l, r)      => evaluate(l)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c)             => prefix + c.toString
    case Variable(c)             => prefix + c.toString
    case UMinus(r)               => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)              => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r)             => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r)             => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)               => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)               => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case binaryConditional(l, r) => binaryConditionalBuild(prefix, "if", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Assignment(l, r)        => assignmentBuild(prefix, ";", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    //case Statement(l) =>
    //case Assignment(l, r)
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(EOL)
    result.append(INDENT)
    result.append(nodeString)
    result.append("(")
    result.append(leftString)
    result.append(")")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def binaryConditionalBuild(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(leftString)
    result.append(")")
    result.append("{")
    result.append(EOL)
    result.append(rightString)
    result.append(EOL)
    result.append("}")
    result.toString
  }

  def assignmentBuild(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(leftString)
    result.append(" =")
    result.append(rightString)
    result.append(nodeString)
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def buildStatementString(prefix: String, postfix: String) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(EOL)
    result.append(prefix)
    //result.append
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "
  val DEDENT = "  "
}
