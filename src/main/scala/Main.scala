package weigl

import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new WhileParser("fun main () {int a = 1+2*3;}")
    Console.out.flush()
    parser.Program.run() match {
      case Success(p:Program) => println(Printer.print(p))
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => println("Unexpected error during parsing run: " + e)
    }
  }
}
