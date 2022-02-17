package weigl

object Printer {
  def print(n: Node): String =
    n match {
      case Program(functions) => functions.map(print).mkString("\n\n")
      case Func(name, arguments, body) => {
        val args = arguments.map(p => p._1 + " " + p._2).mkString(", ")
        s"fun ${name}(${args}) " + print(body)
      }
      case Assignment(variable, expression, typed) =>
        if (typed.isEmpty) s"$variable = ${print(expression)};"
        else s"${typed.get} $variable = ${print(expression)}"
      case If(condition, thenBody, elseBody) => {
        val otherwise :Option[String] = elseBody.map((a: Statement) => ("else" + print(a)))
        s"if(${print(condition)}) ${print(thenBody)}" + otherwise.getOrElse("")
      }
      case While(cond, body) =>
        s"while(${print(cond)}) ${print(body)}"
      case Block(statements) =>
        s"{${statements.map(print).map("\n    " + _).mkString("")}\n}"
      case BinaryExpr(left, right, op) => s"(${print(left)} ${op.symbol} ${print(right)})"
      case UnaryExpr(right, op) => s"($op $right)"
      case IntLiteral(value) => value.toString
      case BoolLiteral(value) => value.toString
      case FunctionCall(function, args) => s"${function}(${args.map(print).mkString(", ")})"
    }
}
