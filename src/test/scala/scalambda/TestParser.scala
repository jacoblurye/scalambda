package scalambda

import org.scalatest.{FlatSpec, Matchers}

class LambdaCalcParserSpec extends FlatSpec with Matchers {
    val parser: LambdaCalcParser = new LambdaCalcParser
    val p = parser.parse _
    val rp = parser.revparse _

    "A LambdaCalcParser" should "parse variables" in {
        p("x") should be (LVar("x"))
        p("xy") should be (LVar("xy"))
        p("xY") should be (LVar("xY"))
        p("x_Y_9_z") should be (LVar("x_Y_9_z"))
        p("(x)") should be (LVar("x"))
    }
    it should "parse simple applications" in {
        p("x y") should be (LApp(LVar("x"), LVar("y")))
        p("x y z") should be (LApp(LVar("x"), LApp(LVar("y"), LVar("z"))))
        p("(((x)) ((y)))") should be (LApp(LVar("x"), LVar("y")))
    }
    it should "parse simple lambda functions" in {
        p("/x. x") should be (LLam("x", LVar("x")))
    }
    it should "parse complex expressions" in {
        p("/x./y. x y") should be (LLam("x", LLam("y", LApp(LVar("x"), LVar("y")))))
        p("(/x.(/y. x y) (/z.z) x) /x.(/y. x y) (/z.z) x") should be (
            LApp(
                LLam("x", LApp(LLam("y", LApp(LVar("x"), LVar("y"))), LApp(LLam("z", LVar("z")), LVar("x")))),
                LLam("x", LApp(LLam("y", LApp(LVar("x"), LVar("y"))), LApp(LLam("z", LVar("z")), LVar("x"))))
            )
        )
    }
    it should "convert ASTs to strings" in {
        p(rp(LVar("x"))) should be (LVar("x"))
        p(rp(LLam("x", LVar("x")))) should be (LLam("x", LVar("x")))
        p(rp(LApp(LVar("x"), LVar("y")))) should be (LApp(LVar("x"), LVar("y")))
    }
}