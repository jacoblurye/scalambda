package scalambda

import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {
  val intptr = new LambdaCalcInterpreter()

  "A LambdaCalcInterpreter" should "correctly perform variable substitutions" in {
    val x = LVar("x")
    val y = LVar("y")
    val z = LVar("z")
    intptr.subst(x, "x", z) should be (z)
    intptr.subst(LLam("x", x), "x", z) should be (z)
    intptr.subst(LLam("x", z), "z", y) should be (LLam("x", y))
    intptr.subst(LApp(z, LLam("x", z)), "z", y) should be (LApp(y, LLam("x", y)))
    intptr.subst(LApp(LLam("x", z), z), "z", y) should be (LApp(LLam("x", y), y))
  }
}