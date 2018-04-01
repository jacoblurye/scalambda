package scalambda

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {
    val parser: LambdaCalcParser = new LambdaCalcParser
    val parse = parser.parse _

    "A LambdaCalcParser" should "parse variables" in {
        parse("x") should be (LVar("x"))
        parse("xy") should be (LVar("xy"))
        parse("xY") should be (LVar("xY"))
        parse("x_Y_9_z") should be (LVar("x_Y_9_z"))
    }
    it should "parse simple applications" in {
        parse("x y") should be (LApp(LVar("x"), LVar("y")))
        parse("x y z") should be (LApp(LVar("x"), LApp(LVar("y"), LVar("z"))))
    }
    it should "parse simple lambda functions" in {
        parser.parse("\\x. x") should be (LLam("x", LVar("x")))
    }
    it should "parse complex expressions" in {
        parser.parse("\\x.\\y. x y") should be (LLam("x", LLam("y", LApp(LVar("x"), LVar("y")))))
    }
}