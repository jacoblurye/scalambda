package scalambda

import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {
  val i = new LambdaCalcInterpreter
  val parser: LambdaCalcParser = new LambdaCalcParser
  val p = parser.parse _

  "A LambdaCalcInterpreter" should "substitute variables in expressions" in {
    val x = LVar("x")
    val y = LVar("y")
    val z = LVar("z")
    i.subst(x, "x", z) should be (z)
    i.subst(LLam("x", x), "x", z) should be (LLam("x", x))
    i.subst(LLam("x", z), "z", y) should be (LLam("x", y))
    i.subst(LApp(z, LLam("x", z)), "z", y) should be (LApp(y, LLam("x", y)))
    i.subst(LApp(LLam("x", z), z), "z", y) should be (LApp(LLam("x", y), y))
  }
  it should "reduce expressions one small-step further if possible" in {
    i.reduce(p("(/x.x) y")) should be (Some(p("y")))
    i.reduce(p("y (/x.x)")) should be (None)
    i.reduce(p("(/x.x) (/y.y)")) should be (Some(p("/y.y")))
    i.reduce(p("(/x.x) ((/y.y) z)")) should be (Some(p("(/x.x) z")))
    i.reduce(p("(/x.(/z.z) x) (/y.y)")) should be (Some(p("(/x.x) (/y.y)")))
    i.reduce(p("(/x.(/z.z) (/x.x)) (/y.y)")) should be (Some(p("(/x./x.x) (/y.y)")))
  }
  it should "evaluate expressions completely" in {
    i.eval("/x./y.x y") should be ("/x./y.x y")
    i.eval("(/x./y.x y) z1 z2") should be ("z1 z2")
    i.eval("(/x./y.y x) z1 z2") should be ("z2 z1")
    i.eval("(/x./y./z.y) z1 z2 z3") should be ("z2")
    i.eval("(/x./y.y x) z1 (/z2.z2 z2)") should be ("z1 z1")
    i.eval("let x = /y.y in x z") should be ("z")
    i.eval("""let plus = /m./n./f./x.m f (n f x) in 
          let 1 = /f./x.f x in plus 1 1""") should be ("/f./x.f (f x)")
  }
}