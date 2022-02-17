package weigl

sealed abstract class Operator(val symbol : String) {

}

object Plus extends Operator("+")
object Minus extends Operator("-")
object Divide extends Operator("/")
object Mult extends Operator("*")
object Negate extends Operator("!")
object Modulo extends Operator("%")
object Less extends Operator("<")
object Greater extends Operator(">")
object LessThan extends Operator("<=")
object GreaterThan extends Operator(">=")
object Or extends Operator("||")
object And extends Operator("&&")


abstract sealed class Node() {
  var position: Option[String] = None
}

case class Program(functions : List[Func]) extends Node
case class Func(name: String, arguments: List[(String, String)], body:Statement) extends Node

abstract sealed class Statement() extends Node
case class Assignment(variable: String, expression: Expr, typed : Option[String]) extends Statement
case class If(condition: Expr, thenBody:Statement, elseBody: Option[Statement]) extends Statement
case class While(condition: Expr, body :Statement) extends Statement
case class Block(statements: List[Statement]) extends Statement

abstract sealed class Expr() extends Node
case class BinaryExpr(left: Expr, right: Expr, op: Operator) extends Expr
case class UnaryExpr(right: Expr, op: Operator) extends Expr
case class IntLiteral(value: Int) extends Expr
case class BoolLiteral(value: Boolean) extends Expr
case class Var(name: String) extends Expr
case class FunctionCall(function: String, args: List[Expr]) extends Expr
