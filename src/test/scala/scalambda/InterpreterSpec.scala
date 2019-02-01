package scalambda

import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {
  val i = Interpreter
  val parser = LambdaCalcParser
  val p = parser.unsafeParse _

  "A LambdaCalcInterpreter" should "substitute variables in expressions" in {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    i.subst(x, "x", z) should be(z)
    i.subst(Lam("x", x), "x", z) should be(Lam("x", x))
    i.subst(Lam("x", z), "z", y) should be(Lam("x", y))
    i.subst(LApp(z, Lam("x", z)), "z", y) should be(LApp(y, Lam("x", y)))
    i.subst(LApp(Lam("x", z), z), "z", y) should be(LApp(Lam("x", y), y))
  }
  it should "reduce expressions one small-step further if possible" in {
    i.reduce(p("((/x.x) y)")) should be(Some(p("y")))
    i.reduce(p("(y (/x.x))")) should be(None)
    i.reduce(p("((/x.x) (/y.y))")) should be(Some(p("/y.y")))
    i.reduce(p("((/x.x) ((/y.y) z))")) should be(Some(p("((/y.y) z)")))
    i.reduce(p("((/x.(/z.z) x) (/y.y))")) should be(Some(p("((/z.z) (/y.y))")))
    i.reduce(p("((/x.((/z.z) (/x.x))) (/y.y))")) should be(
      Some(p("((/z.z) (/x.x))")))
  }
  it should "evaluate expressions completely" in {
    i.eval("/x./y.(x y)") should be("/x./y.(x y)")
    i.eval("((/x./y.(x y)) z1 z2)") should be("(z1 z2)")
    i.eval("((/x./y.(y x)) z1 z2)") should be("(z2 z1)")
    i.eval("((/x./y./z.y) z1 z2 z3)") should be("z2")
    i.eval("((/x./y.(y x)) z1 (/z2.(z2 z2)))") should be("(z1 z1)")
    i.eval("let x = /y.y in (x z)") should be("z")
    i.eval("let x = /y.y in let y = /f./z.(f z b) in ((y x) /a.a)") should be(
      "b")
    i.eval("""let succ = /n./f./x.((n f) (f x)) in 
              let 1 = /f./x.(f x) in (succ 1)""") should be("/f./x.(f (f x))")
    i.eval("""let plus = /m./n./f./x.((m f) (n f x)) in 
              let 1 = /f./x.(f x) in (plus 1 1)""") should be("/f./x.(f (f x))")
  }
}
