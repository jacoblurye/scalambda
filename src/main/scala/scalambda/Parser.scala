package scalambda

import scala.util.parsing.combinator.RegexParsers

/*
 * Given a text file, yield the corresponding lambda calculus AST.
 */
class LambdaCalcParser extends RegexParsers {

    /** Grammer definition */
    def exp: Parser[LExp] =  lvar | lam | letexp | "(" ~> app <~ ")"
    def lvar: Parser[LExp] = id ^^ {LVar(_)}
    def lam:Parser[LExp] = "/" ~ id ~ "." ~ exp ^^ {
        case "/" ~ x ~ "." ~ e => LLam(x, e)
    }
    def letexp:Parser[LExp] = "let" ~ id ~ "=" ~ exp ~ "in" ~ exp ^^ {
        case "let" ~ x ~ "=" ~ e1 ~ "in" ~ e2 => LLet(x, e1, e2)
    }
    def app: Parser[LExp] = rep1(exp) ^^ {
        case e::es => es.foldLeft(e){(func, arg) => LApp(func, arg)}
    }

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
    def parse(expression: String): Option[LExp] = {
        parseAll(exp, expression) match {
            case Success(tree, _) => Some(tree)
            case _: NoSuccess => None
        }
    }

    def noopt_parse(expression: String): LExp = {
        parseAll(exp, expression) match {
            case Success(tree, _) => tree
            case _: NoSuccess => throw new Exception("cannot parse expression " + expression)
        }
    }

    /** Outputs valid string given AST */
    def revparse(exp: LExp): String = {
        exp match {
            case LVar(x) => x
            case LLam(x, e) => "/" + x + "." + revparse(e)
            case LApp(e1, e2) => "(" + revparse(e1) + " " + revparse(e2) + ")"
            case LLet(x, e1, e2) => 
                "let " + x + " = " + revparse(e1) + " in " + revparse(e2)
        }
    }
}