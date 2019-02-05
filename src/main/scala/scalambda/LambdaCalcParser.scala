package scalambda

import java.io.FileNotFoundException

import scala.io.Source
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
  * Parse a string representation of a lambda calculus expression
  * to its corresponding lambda calculus abstract syntax tree.
  *
  * Implements the grammar:
  * {{{
  * <expression> ::= <variable> | <lambda> | <letexp> | "(" <app> ")"
  * <app>        ::= <expression> <expression>
  * <lambda>     ::= "/" <id> "." <expression>
  * <letexp>     ::= "let" <id> "=" <expression> "in" <expression>
  * <variable>   ::= <id>
  * <id>         ::= "a" | ... | "z" | "A" | ... | "Z" | "0" | ... | "9 | "_"  // with reserved keyword "let" and "in"
  * }}}
  *
  */
trait LambdaCalcParsers extends RegexParsers {
  def expression: Parser[Exp] = variable | lambda | letexp | "(" ~> app <~ ")"
  def variable: Parser[Exp] = id ^^ { Var }
  def lambda: Parser[Exp] = "/" ~ id ~ "." ~ expression ^^ {
    case "/" ~ x ~ "." ~ e => Lam(x, e)
  }
  def letexp: Parser[Exp] =
    "let" ~ id ~ "=" ~ expression ~ "in" ~ expression ^^ {
      case "let" ~ x ~ "=" ~ e1 ~ "in" ~ e2 => Let(x, e1, e2)
    }
  def app: Parser[Exp] = rep1(expression) ^^ {
    case e :: es =>
      es.foldLeft(e) { (func, arg) =>
        App(func, arg)
      }
    case nil => ??? // TODO: fail with "encountered empty application"
  }
  def reserved: Set[String] = Set("let", "in")
  def idRegex: Parser[String] = "[a-zA-Z0-9_]+".r
  def id: Parser[String] =
    Parser(
      input =>
        idRegex(input).filterWithError(
          !reserved.contains(_),
          resword => s"Cannot use keyword $resword as identifier",
          input
      ))

  def definition: Parser[(String, Exp)] = id ~ "=" ~ expression ^^ {
    case x ~ "=" ~ e => (x, e)
  }

}

object LambdaCalcParser extends LambdaCalcParsers {

  /** Outputs the string representation of an expression */
  def revparse(exp: Exp): String = exp.toString

  /** Attempts to parse a string input into an expression */
  def parse(input: String): Option[Exp] = {
    parseAll(expression, input) match {
      case Success(tree, _) => Some(tree)
      case _                => None
    }
  }

  /** Load identifier definitions from a file.
    *
    * @param path location of definition file.
    * @return a sequence of tuples containing identifiers and
    *         their parsed definitions.
    */
  def loadDefinitionsFile(path: String): Seq[(String, Exp)] = {
    assert(path.endsWith(".lmb"), "expected file with extension \".lmb\"")

    val source =
      // Look for file in local filesystem
      Try(Source.fromFile(path)).toOption
        .getOrElse(
          // Look for file on the classpath
          Try(Source.fromResource(path)).toOption
            .getOrElse(
              throw new FileNotFoundException(path)
            )
        )

    val lines =
      Try(source.getLines).getOrElse(throw new FileNotFoundException(path))

    lines
      .foldLeft(Seq.empty[(String, Exp)]) { (defs, line) =>
        defs :+ parse(definition, line).get
      }
  }
}
