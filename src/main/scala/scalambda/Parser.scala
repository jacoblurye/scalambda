package scalambda

import scala.util.parsing.combinator.RegexParsers

/*
 * Given a text file, yield the corresponding lambda calculus AST.
 */
class LambdaCalcParser extends RegexParsers {
    def exp: Parser[LExp] = app | "(" ~> exp <~ ")" | lam | lvar
    def app: Parser[LExp] = ("(" ~> exp <~ ")" | lvar) ~ exp ^^ {
        case e1 ~ e2 => LApp(e1, e2)
    }
    def lam: Parser[LExp] = "\\" ~ id ~ "." ~ exp ^^ {
        case "\\" ~ x ~ "." ~ e => LLam(x, e)
    }
    def lvar: Parser[LExp] = id ^^ {LVar(_)}
    def id:  Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

    def parse(expression: String) = {
        parseAll(exp, expression) match {
            case Success(tree, _) => tree
            case e: NoSuccess => throw new IllegalArgumentException(e.toString())
        }
    }
}