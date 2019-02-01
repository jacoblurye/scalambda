package scalambda

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
  * Given a string representing a lambda calculus expression,
  * generate the corresponding lambda calculus AST.
  */
class LambdaCalcParsers extends RegexParsers {
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
        LApp(func, arg)
      }
    case nil => ??? // TODO: resolve this
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

  def defn: Parser[(String, Exp)] = id ~ "=" ~ expression ^^ {
    case x ~ "=" ~ e => (x, e)
  }

}

object LambdaCalcParser extends LambdaCalcParsers {

  /** Outputs valid string given AST */
  def revparse(exp: Exp): String = {
    exp match {
      case Var(x)       => x
      case Lam(x, e)    => "/" + x + "." + revparse(e)
      case LApp(e1, e2) => "(" + revparse(e1) + " " + revparse(e2) + ")"
      case Let(x, e1, e2) =>
        "let " + x + " = " + revparse(e1) + " in " + revparse(e2)
    }
  }

  def parse(input: String): Option[Exp] = {
    parseAll(expression, input) match {
      case Success(tree, _) => Some(tree)
      case _                => None
    }
  }

  def unsafeParse(input: String): Exp = parse(input).get

  def parseDefinitionFile(fname: String): Map[String, Exp] = {
    val lines = Source.fromFile(fname).getLines.toList.reverse
    lines.foldLeft(Map.empty[String, Exp]) { (map, line) =>
      map + parse(defn, line).get
    }
  }
}
