package scalambda

import org.scalatest.{FunSuite, Matchers}

class LambdaCalcParserSpec extends FunSuite with Matchers {
  val p: String => Exp = LambdaCalcParser.parse(_).get
  val r: Exp => String = LambdaCalcParser.revparse

  test("parse variables") {
    p("x") should be(Var("x"))
    p("xy") should be(Var("xy"))
    p("xY") should be(Var("xY"))
    p("x_Y_9_z") should be(Var("x_Y_9_z"))
    p("(x)") should be(Var("x"))
    p("1") should be(Var("1"))
  }
  test("simple applications") {
    p("(x y)") should be(App(Var("x"), Var("y")))
    p("(x y z)") should be(App(App(Var("x"), Var("y")), Var("z")))
    p("(((x)) ((y)))") should be(App(Var("x"), Var("y")))
    p("(a (b c))") should be(App(Var("a"), App(Var("b"), Var("c"))))
  }
  test("simple lambda functions") {
    p("/x. x") should be(Lam("x", Var("x")))
  }
  test("simple let expressions") {
    p("let x = /y.y in x") should be(Let("x", Lam("y", Var("y")), Var("x")))
    p("let x = y in let z = q in (x z)") should be
    Let("x", Var("y"), Let("z", Var("q"), App(Var("x"), Var("z"))))
  }
  test("complex expressions") {
    p("/x./y. (x y)") should be(Lam("x", Lam("y", App(Var("x"), Var("y")))))
    p("/x.(/y.(x y) (/z.z) x)") should be(
      Lam("x",
          App(App(Lam("y", App(Var("x"), Var("y"))), Lam("z", Var("z"))),
              Var("x")))
    )
    p("/x./y.(x y (/z.z x))") should be(
      Lam("x",
          Lam("y",
              App(App(Var("x"), Var("y")), App(Lam("z", Var("z")), Var("x")))))
    )
    p("(/x.(/y.(x y) (/z.z) x) /x./y.(x y (/z.z x)))") should be(
      App(
        Lam("x",
            App(App(Lam("y", App(Var("x"), Var("y"))), Lam("z", Var("z"))),
                Var("x"))),
        Lam("x",
            Lam("y",
                App(App(Var("x"), Var("y")),
                    App(Lam("z", Var("z")), Var("x")))))
      )
    )
  }
  test("reverse parsing") {
    p(r(Var("x"))) should be(Var("x"))
    p(r(Lam("x", Var("x")))) should be(Lam("x", Var("x")))
    p(r(App(Var("x"), Var("y")))) should be(App(Var("x"), Var("y")))
    p(r(App(App(Var("x"), Var("y")), Lam("x", Var("x"))))) should be
    App(App(Var("x"), Var("y")), Lam("x", Var("x")))
  }

  test("definition loading") {
    val path = getClass.getResource("/testlib.lmb").getPath

    LambdaCalcParser.loadDefinitionsFile(path) should contain theSameElementsAs
      Seq[(String, Exp)](
        ("true", Lam("x", Lam("y", Var("x")))),
        ("false", Lam("x", Lam("y", Var("y")))),
        ("0", Lam("f", Lam("x", Var("x"))))
      )
  }
}
