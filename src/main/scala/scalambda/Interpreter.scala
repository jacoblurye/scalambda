package scalambda

/**
  * Evaluate a given LExp instance accord to call-by-value semantics.
  */
class LambdaCalcInterpreter {

  /** Generates unused variable id given set of used ids */
  private def genId(used: Set[String]) = {
    var suff: Int = 0
    var name = "x" + suff
    while (used(name)) {
      suff += 1
      name = "x" + suff
    }
    name
  }

  /** Finds the set of free variables in a lambda calculus expression. */
  private def findFVs(e: LExp,
                      fvs: Set[String] = Set.empty,
                      cvs: Set[String] = Set.empty): Set[String] = {
    e match {
      case LVar(x) => if (cvs(x)) Set.empty else fvs + x
      case LLam(x, e) => findFVs(e, fvs, cvs + x)
      case LApp(e1, e2) => findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs)
    }
  }

  /** Replace all instances of x in e1 with e2. */
  private def subst(e1: LExp, x: String, e2: LExp): LExp = {
    e1 match {
      case LVar(s) => if (x == s) e2 else e1
      case LApp(t1, t2) => LApp(subst(t1, x, e2), subst(t2, x, e2))
      case LLam(s, e) =>
        if (x == s) 
          e2
        else  {
          val e2FVs = findFVs(e2)
          if (!e2FVs(x)) 
            LLam(x, subst(e, x, e2))
          else {
            val eFVs = findFVs(e)
            val usedVars = (eFVs union e2FVs) + x
            val freshVar = genId(usedVars)
            LLam(freshVar, subst(subst(e, x, LVar(freshVar)), s, e2))
          }
        }
    }
  }
}