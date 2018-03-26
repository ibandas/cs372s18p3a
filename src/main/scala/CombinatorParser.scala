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
  //NOTE: The blocks in statements were changed from "statement". Maybe change back?
  //TODO: Add implementation of ";", conditional, and block (expression, assignment, and loop is done already)
  def statement: Parser[Expr] = (
    ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
    | "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case g ~ _ ~ b => While(g, b) }
    | "{" ~> repsep(block, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  )

  //TODO: assignment ::= ident "=" expression ";"
  def assignment: Parser[Expr] = (
    ident ~ "=" ~ expr ~ ";" ^^ {case g ~ _ ~ b => While(g, b)}
  )

  //TODO: conditional ::= "if" "(" expression ")" block [ "else" block ] (FIX Case)
  def conditional: Parser[Expr] = (
    "if" ~ "(" ~> expr ~ ")" ~ block ~ "[" ~ "else" ~ block ~ "]" ^^ {case g ~ _ ~ b => While(g, b)}
  )

  //TODO: loop ::= "while" "(" expression ")" block
  def loop: Parser[Expr] = (
    "while" ~ "(" ~> expr ~ ")" ~ block ^^ {case g ~ _ ~ b => While(g, b)}
  )

  //TODO: block ::= "{" statement* "}"
  def block: Parser[Expr] = (
    "{" ~ statement ~ "}" ^^ {case s ~ _ ~ r => Sequence()}
  )
}
