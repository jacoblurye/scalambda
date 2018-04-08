package scalambda

import scala.util.parsing.combinator.RegexParsers

/*
 * Given a text file, yield the corresponding lambda calculus AST.
 */
class LambdaCalcParser extends RegexParsers {

    /** Grammer definition */
    def exp: Parser[LExp] = app | "(" ~> exp <~ ")" | lam | lvar | let
    def let: Parser[LExp] = "let" ~ id ~ "=" ~ exp ~ "in" ~ exp ^^ {
        case "let" ~ x ~ "=" ~ e1 ~ "in" ~ e2 => LLet(x, e1, e2)
    }
    def app: Parser[LExp] = ("(" ~> exp <~ ")" | lvar) ~ exp ^^ {
        case e1 ~ e2 => LApp(e1, e2)
    }
    def lam: Parser[LExp] = "/" ~ id ~ "." ~ exp ^^ {
        case "/" ~ x ~ "." ~ e => LLam(x, e)
    }
    def lvar: Parser[LExp] = id ^^ {LVar(_)}
    def id:  Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

    /** Outputs lambda calc AST given a string adhering to grammar */
    def parse(expression: String): LExp = {
        parseAll(exp, expression) match {
            case Success(tree, _) => tree
            case e: NoSuccess => throw new IllegalArgumentException(e.toString())
        }
    }

    /** Outputs valid string given AST */
    def revparse(exp: LExp): String = {
        exp match {
            case LVar(x) => x
            case LLam(x, e) => "/" + x + "." + revparse(e)
            case LApp(e1, e2) => e1 match {
                case LApp(_,_) => "(" + revparse(e1) + ") " + revparse(e2)
                case _ => revparse(e1) + " " + revparse(e2)
            }
            case LLet(x, e1, e2) => 
                "let " + x + " = " + revparse(e1) + " in " + revparse(e2)
        }
    }
}