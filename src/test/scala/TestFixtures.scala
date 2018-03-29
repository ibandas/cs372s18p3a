package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    );

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val binaryConditionalString = "if (1) { x = 2; }"

  val binaryConditionalStatement = binaryConditional(Constant(1), Sequence(Assignment(Variable("x"), Constant(2))))

  val assignmentString = "x = 5;"

  val assignmentStatement = Assignment(Variable("x"), Constant(5))

  val simpleStatement = Div(Minus(Plus(Constant(1), Variable("y2")), Times(Constant(3), Variable("y4"))), Constant(5))

  val simpleString = "((1 + y2) - (3 * y4)) / 5;"

  val triConditional = Conditional(Constant(1), Sequence((Assignment(Variable("x"), Constant(2)))), Sequence((Assignment(Variable("x"), Constant(3)))))

  val triConditionalString = "if (1) { x = 2; } else { x = 3; }"

  val blockStatement = Sequence(Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))), Assignment(Variable("y"), Plus(Variable("y"), Constant(1))))

  val blockString = "{ r = r + x; y = y + 1 ; }"

  val complexAssignmentStatement = Assignment(Variable("x"), Div(Minus(Plus(Constant(1), Variable("y2")), Times(Constant(3), Variable("y4"))), Constant(5)))

  val complexAssignmentString = "x = ((1 + y2) - (3 * y4)) / 5;"

  val complexWhileStatement = While(Variable("y"), Sequence(Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))), Assignment(Variable("y"), Minus(Variable("y"), Constant(1)))))

  val complexWhileString = "while (y) { r = r + x; y = y - 1; }"

  val complexBinaryConditionStatement = binaryConditional(Constant(4), Sequence(Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))), Assignment(Variable("y"), Minus(Variable("y"), Constant(1)))))

  val complexBinaryConditionString = "if (4) { r = r + x; y = y + 1; }"

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    );
}
