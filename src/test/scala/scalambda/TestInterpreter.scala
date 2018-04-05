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
    i
  }
  it should "reduce expressions if possible" in {
    i.reduce(p("(/x.x) y")) should be (Some(p("y")))
    i.reduce(p("y (/x.x)")) should be (None)
    i.reduce(p("(/x.x) (/y.y)")) should be (Some(p("/y.y")))
    i.reduce(p("(/x.x) ((/y.y) z)")) should be (Some(p("(/x.x) z")))
    i.reduce(p("(/x.(/z.z) x) (/y.y)")) should be (Some(p("(/z.z) (/y.y)")))
    i.reduce(p("(/x.(/z.z) (/x.x)) (/y.y)")) should be (Some(p("(/z.z) (/x.x)")))
  }
}