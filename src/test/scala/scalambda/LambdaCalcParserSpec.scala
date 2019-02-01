package scalambda

import org.scalatest.{FlatSpec, Matchers}

class LambdaCalcParserSpec extends FlatSpec with Matchers {
  val p = LambdaCalcParser.unsafeParse _
  val rp = LambdaCalcParser.revparse _

  "A LambdaCalcParser" should "parse variables" in {
    p("x") should be(Var("x"))
    p("xy") should be(Var("xy"))
    p("xY") should be(Var("xY"))
    p("x_Y_9_z") should be(Var("x_Y_9_z"))
    p("(x)") should be(Var("x"))
    p("1") should be(Var("1"))
  }
  it should "parse simple applications" in {
    p("(x y)") should be(LApp(Var("x"), Var("y")))
    p("(x y z)") should be(LApp(LApp(Var("x"), Var("y")), Var("z")))
    p("(((x)) ((y)))") should be(LApp(Var("x"), Var("y")))
    p("(a (b c))") should be(LApp(Var("a"), LApp(Var("b"), Var("c"))))
  }
  it should "parse simple lambda functions" in {
    p("/x. x") should be(Lam("x", Var("x")))
  }
  it should "parse simple let expressions" in {
    p("let x = /y.y in x") should be(Let("x", Lam("y", Var("y")), Var("x")))
    p("let x = y in let z = q in (x z)") should be
    (Let("x", Var("y"), Let("z", Var("q"), LApp(Var("x"), Var("z")))))
  }
  it should "parse complex expressions" in {
    p("/x./y. (x y)") should be(Lam("x", Lam("y", LApp(Var("x"), Var("y")))))
    p("/x.(/y.(x y) (/z.z) x)") should be(
      Lam("x",
          LApp(LApp(Lam("y", LApp(Var("x"), Var("y"))), Lam("z", Var("z"))),
               Var("x")))
    )
    p("/x./y.(x y (/z.z x))") should be(
      Lam("x",
          Lam("y",
              LApp(LApp(Var("x"), Var("y")),
                   LApp(Lam("z", Var("z")), Var("x")))))
    )
    p("(/x.(/y.(x y) (/z.z) x) /x./y.(x y (/z.z x)))") should be(
      LApp(
        Lam("x",
            LApp(LApp(Lam("y", LApp(Var("x"), Var("y"))), Lam("z", Var("z"))),
                 Var("x"))),
        Lam("x",
            Lam("y",
                LApp(LApp(Var("x"), Var("y")),
                     LApp(Lam("z", Var("z")), Var("x")))))
      )
    )
  }
  it should "convert ASTs to strings" in {
    p(rp(Var("x"))) should be(Var("x"))
    p(rp(Lam("x", Var("x")))) should be(Lam("x", Var("x")))
    p(rp(LApp(Var("x"), Var("y")))) should be(LApp(Var("x"), Var("y")))
    p(rp(LApp(LApp(Var("x"), Var("y")), Lam("x", Var("x"))))) should be
    LApp(LApp(Var("x"), Var("y")), Lam("x", Var("x")))
  }

  it should "parse definitions from files" in {
    val testFile = "defs/testlib.lmb"

    LambdaCalcParser.parseDefinitionFile(testFile) should be(
      Map[String, Exp](
        "true" -> Lam("x", Lam("y", Var("x"))),
        "false" -> Lam("x", Lam("y", Var("y"))),
        "0" -> Lam("f", Lam("x", Var("x")))
      ))
  }
}
