package scalambda

import org.scalatest.{FunSuite, Matchers}

class ExpSpec extends FunSuite with Matchers {
  val r: Exp => String = LambdaCalcParser.reverseParse
  val p: String => Exp = LambdaCalcParser.parse(_).get

  test("free") {
    Var("x").free() should contain theSameElementsAs Set("x")

    Lam("x", Var("x")).free() shouldBe empty
    Lam("x", Var("y")).free() should contain theSameElementsAs Set("y")
    Lam("x", Lam("y", Var("z"))).free() should contain theSameElementsAs Set(
      "z")

    App(Var("x"), Var("y")).free() should contain theSameElementsAs Set("x",
                                                                        "y")

    App(Lam("x", Var("x")), Var("y"))
      .free() should contain theSameElementsAs Set("y")
    App(Lam("x", Var("y")), Var("x"))
      .free() should contain theSameElementsAs Set("x", "y")
    Lam("y", App(Lam("x", Var("y")), Var("x")))
      .free() should contain theSameElementsAs Set("x")
  }

  test("substitute") {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    x.substitute("x", z) shouldBe z
    Lam("x", x).substitute("x", z) shouldBe Lam("x", x)
    Lam("x", z).substitute("z", y) shouldBe Lam("x", y)
    App(z, Lam("x", z)).substitute("z", y) shouldBe App(y, Lam("x", y))
    App(Lam("x", z), z).substitute("z", y) shouldBe App(Lam("x", y), y)
  }

  test("reduce") {
    p("((/x.x) y)").reduce shouldBe Some(p("y"))
    p("(y (/x.x))").reduce shouldBe None
    p("((/x.x) (/y.y))").reduce shouldBe Some(p("/y.y"))
    p("((/x.x) ((/y.y) z))").reduce shouldBe Some(p("((/y.y) z)"))
    p("((/x.(/z.z) x) (/y.y))").reduce shouldBe Some(p("((/z.z) (/y.y))"))
    p("((/x.((/z.z) (/x.x))) (/y.y))").reduce shouldBe
      Some(p("((/z.z) (/x.x))"))
  }

  test("normalForm") {
    r(p("/x./y.(x y)").normalForm()) shouldBe "/x./y.(x y)"
    r(p("((/x./y.(x y)) z1 z2)").normalForm()) shouldBe "(z1 z2)"
    r(p("((/x./y.(y x)) z1 z2)").normalForm()) shouldBe "(z2 z1)"
    r(p("((/x./y./z.y) z1 z2 z3)").normalForm()) shouldBe "z2"
    r(p("((/x./y.(y x)) z1 (/z2.(z2 z2)))").normalForm()) shouldBe "(z1 z1)"
    r(p("let x = /y.y in (x z)").normalForm()) shouldBe "z"
    r(p("let x = /y.y in let y = /f./z.(f z b) in ((y x) /a.a)").normalForm()) shouldBe
      "b"
    r(p("""let succ = /n./f./x.((n f) (f x)) in
              let 1 = /f./x.(f x) in (succ 1)""").normalForm()) shouldBe "/f./x.(f (f x))"
    r(p("""let plus = /m./n./f./x.((m f) (n f x)) in
              let 1 = /f./x.(f x) in (plus 1 1)""").normalForm()) shouldBe "/f./x.(f (f x))"
  }

  test("normalForm on definitions") {
    val defs = LambdaCalcParser.loadDefinitionsFile("core.lmb").toMap

    App(defs("iszero"), defs("0")).normalForm() shouldBe defs("true")
    App(defs("iszero"), defs("1")).normalForm() shouldBe defs("false")
    App(defs("pred"), defs("2")).normalForm() shouldBe defs("1")
    App(App(defs("succ"), defs("1")), defs("1"))
      .normalForm(debug = true) shouldBe defs("1")
    App(defs("times"), defs("0")).normalForm() shouldBe defs("1")
    App(defs("fact"), defs("1")).normalForm() shouldBe defs("1")
    App(defs("fact"), defs("2")).normalForm() shouldBe defs("2")
    App(defs("fact"), defs("3")).normalForm() shouldBe defs("6")
  }
}
