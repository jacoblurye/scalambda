package scalambda

import scala.util.parsing.combinator.RegexParsers

/*
 * Given a text file, yield the corresponding lambda calculus AST.
 */
class LambdaCalcParser extends RegexParsers {

    /** Grammer definition */
    def exp: Parser[LExp] =  letexp | app | parenexp | lam | lvar
    def parenexp: Parser[LExp] = "(" ~> exp <~ ")" ^^ {
        case e if e.isInstanceOf[LApp] => e.splittable = false; e
        case e => e
    }
    def app: Parser[LExp] = (lvar | parenexp) ~ exp ^^ {
        case e1 ~ e2 => e2 match {
            case LApp(h, t) if (e2.splittable) => LApp(LApp(e1, h), t)
            case _ => LApp(e1, e2)
        }
    }
    def letexp: Parser[LExp] = "let" ~ id ~ "=" ~ exp ~ "in" ~ exp ^^ {
        case "let" ~ x ~ "=" ~ e1 ~ "in" ~ e2 => LLet(x, e1, e2)
    }
    def lam: Parser[LExp] = "/" ~ id ~ "." ~ exp ^^ {
        case "/" ~ x ~ "." ~ e => LLam(x, e)
    }
    def lvar: Parser[LExp] = id ^^ {LVar(_)}

    /**
      * Accept as valid identifiers any alphanumeric string (with underscores)
      * that is not a reserved keyord.
      */
    def reserved = Set("let", "in")
    def idRegex: Parser[String] = "[a-zA-Z0-9_]+".r
    def id: Parser[String] = Parser(input =>
      idRegex(input).filterWithError(
        !reserved.contains(_),
        resword => s"Cannot use keyword $resword as identifier",
        input
      )
    )

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
            case LApp(e1, e2) => e2 match {
                case LApp(_,_) => revparse(e1) + " (" + revparse(e2) + ")"
                case _ => revparse(e1) + " " + revparse(e2)
            }
            case LLet(x, e1, e2) => 
                "let " + x + " = " + revparse(e1) + " in " + revparse(e2)
        }
    }
}