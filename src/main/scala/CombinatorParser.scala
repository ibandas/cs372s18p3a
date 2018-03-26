package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

//Ident and wholeNumber come from JavaTokenParsers
object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= ident | wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case s => Variable(s) }
  )

  /** statement ::= ident = expr | while (expr) statement | { statement , ... , statement } */
  //TODO: Fix case class for expr ";"
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { null }
    | assignment
    | conditional
    | loop
    | block
  )

  //assignment ::= ident "=" expression ";"
  def assignment: Parser[Expr] = (
    ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assignment(Variable(s), r) }
  )

  //conditional ::= "if" "(" expression ")" block [ "else" block ] (FIX Case)
  //TODO: Make it where you don't always need an else
  //It had "~> expr" before. Error went away when switching to "~"
  def conditional: Parser[Expr] = (
    "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block ^^ { case _ ~ _ ~ g ~ _ ~ b ~ _ ~ t => Conditional(g, b, t) }
  )

  //loop ::= "while" "(" expression ")" block
  def loop: Parser[Expr] = (
    "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case g ~ _ ~ b => While(g, b) }
  )

  //block ::= "{" statement* "}"
  def block: Parser[Expr] = (
    "{" ~> repsep(statement, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  )
}

//Below is a version before I changed it some more to what I think works
/*
def statement: Parser[Expr] = (
    ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
    | "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case g ~ _ ~ b => While(g, b) }
    | "{" ~> repsep(block, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  )
 */
