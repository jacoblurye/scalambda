package scalambda

import org.scalatest.{FlatSpec, Matchers}

class LambdaCalcParserSpec extends FlatSpec with Matchers {
    val parser: LambdaCalcParser = new LambdaCalcParser
    val parse = parser.parse _

    "A LambdaCalcParser" should "parse variables" in {
        parse("x") should be (LVar("x"))
        parse("xy") should be (LVar("xy"))
        parse("xY") should be (LVar("xY"))
        parse("x_Y_9_z") should be (LVar("x_Y_9_z"))
        parse("(x)") should be (LVar("x"))
    }
    it should "parse simple applications" in {
        parse("x y") should be (LApp(LVar("x"), LVar("y")))
        parse("x y z") should be (LApp(LVar("x"), LApp(LVar("y"), LVar("z"))))
        parse("(((x)) ((y)))") should be (LApp(LVar("x"), LVar("y")))
    }
    it should "parse simple lambda functions" in {
        parse("\\x. x") should be (LLam("x", LVar("x")))
    }
    it should "parse complex expressions" in {
        parse("\\x.\\y. x y") should be (LLam("x", LLam("y", LApp(LVar("x"), LVar("y")))))
        parse("(\\x.(\\y. x y) (\\z.z) x) \\x.(\\y. x y) (\\z.z) x") should be (
            LApp(
                LLam("x", LApp(LLam("y", LApp(LVar("x"), LVar("y"))), LApp(LLam("z", LVar("z")), LVar("x")))),
                LLam("x", LApp(LLam("y", LApp(LVar("x"), LVar("y"))), LApp(LLam("z", LVar("z")), LVar("x"))))
            )
        )
    }
    it should "convert ASTs back to correct strings" in {

    }
}