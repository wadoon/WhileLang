package weigl

import org.parboiled2._

class WhileParser(val input: ParserInput) extends Parser {
  def Program: Rule1[Program] = rule {
    ws ~ (oneOrMore(Function).separatedBy(ws) ~ ws ~ EOI) ~> ((a: Seq[Func]) => weigl.Program(a.toList))
  }

  def Function: Rule1[Func] = rule {
    ("fun" ~ ws ~ Name ~ ws ~ LPAREN ~ zeroOrMore(Argument).separatedBy(ws(",")) ~ ws ~ RPAREN ~ ws ~ Statement) ~>
      ((n: String, args: Seq[(String, String)], body: Statement) => Func(n, args.toList, body))
  }

  def Argument: Rule1[(String, String)] = rule {
    (Type ~ Name) ~> ((t: String, n: String) => (t, n))
  }

  def Type: Rule1[String] = rule {
    capture("int" | "bool") ~ ws
  }

  def Name: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ ws
  }

  def Digits = rule {
    oneOrMore(CharPredicate.Digit)
  }

  def Statement: Rule1[Statement] = rule {
    Body | If | While | Assign
  }

  def Assign = rule {
    (Type.? ~ Name ~ ws ~ "=" ~ ws ~ Expression ~ ";") ~> ((t: Option[String], n, e) => Assignment(n, e, t))
  }

  def LPAREN = ws("(")
  def RPAREN = ws(")")
  def LBRACE = ws("{")
  def RBRACE = ws("}")
  def ELSE = ws("else")
  def WHILE = ws("while")
  def IF = ws("if")
  def AND = ws("&&")
  def OR = ws("||")
  def BANG = ws("!")
  def PLUS = ws("+")
  def MINUS = ws("-")
  def SLASH = ws("/")
  def PERCENT = ws("%")
  def STAR = ws("*")
  def TRUE = ws("true")
  def FALSE = ws("false")
  def GT = ws(">")
  def LT = ws("<")
  def LTE = ws("<=")
  def GTE = ws(">=")


  def If: Rule1[Statement] = rule {
    (IF ~ LPAREN ~ Expression ~ RPAREN ~ Statement ~ optional(ELSE ~ Statement)) ~> ((e, s, o) => weigl.If(e, s, o))
  }

  def While: Rule1[Statement] = rule {
    (WHILE ~ LPAREN ~ Expression ~ RPAREN ~ Statement) ~> ((e, s) => weigl.While(e, s))
  }

  //def For = rule {"for" ~ "(" ~ ";" ~ Expression ~ ";" ~ ")" ~ "(" ~ Expression ~ ")" ~ Statement }

  def Body: Rule1[Statement] = rule {
    LBRACE ~ (zeroOrMore(Statement) ~> ((a: Seq[Statement]) => Block(a.toList))) ~ RBRACE
  }

  def Expression: Rule1[Expr] = rule {
    Term120
  }

  def Term120: Rule1[Expr] = rule {
    oneOrMore(Term110).separatedBy(AND) ~> ((seq:Seq[Expr]) => seq.reduce((a,b)=>BinaryExpr(a, b, And)))
  }

  def Term110: Rule1[Expr] = rule {
    oneOrMore(Term100).separatedBy(OR) ~> ((seq:Seq[Expr]) => seq.reduce((a, b)=>BinaryExpr(a, b, Or)))
  }

  def Term100: Rule1[Expr] = rule {
    Term90 ~ zeroOrMore(
      LT ~ Term90 ~> ((a: Expr, b) => BinaryExpr(a, b, Less))
        | GT ~ Term90 ~> ((a: Expr, b) => BinaryExpr(a, b, Greater))
        | GTE ~ Term90 ~> ((a: Expr, b) => BinaryExpr(a, b, GreaterThan))
        | LTE ~ Term90 ~> ((a: Expr, b) => BinaryExpr(a, b, LessThan)))
  }

  def Term90: Rule1[Expr] = rule {
    Term80 ~ zeroOrMore(
      PLUS ~ Term80 ~> ((a: Expr, b) => BinaryExpr(a, b, Plus))
        | MINUS ~ Term80 ~> ((a: Expr, b) => BinaryExpr(a, b, Minus)))
  }

  def Term80: Rule1[Expr] = rule {
    Term70 ~ zeroOrMore(
          STAR ~ Term70 ~> ((a: Expr, b) => BinaryExpr(a, b, Mult))
        | SLASH ~ Term70 ~> ((a: Expr, b) => BinaryExpr(a, b, Divide))
        | PERCENT ~ Term70 ~> ((a: Expr, b) => BinaryExpr(a, b, Modulo)))
  }

  def Term70: Rule1[Expr] = rule {
    ( BANG ~ Factor ~> (a => UnaryExpr(a, Negate))
    | MINUS ~ Factor ~> (a => UnaryExpr(a, Minus))
    | Factor
    )
  }

  def Factor: Rule1[Expr] = rule {
    Number | Boolean | Parens
  }

  def Parens: Rule1[Expr] = rule {
    LPAREN ~ Expression ~ RPAREN
  }

  def Number = rule {
    capture(Digits) ~> (s => IntLiteral(s.toInt))
  }

  def Boolean = rule {
    TRUE ~ push(BoolLiteral(true)) | FALSE ~ push(BoolLiteral(false))
  }

  def WhiteSpace = rule(zeroOrMore(CharPredicate(" \n\r\t\f")))

  def ws = rule(quiet(WhiteSpace))

  def ws(c: String) = rule(c ~ quiet(WhiteSpace))
}

