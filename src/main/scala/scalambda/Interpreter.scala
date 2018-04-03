package scalambda

/**
  * Evaluate a given LExp instance accord to call-by-value semantics.
  */
class LambdaCalcInterpreter {

  /** Generates unused variable id given set of used ids */
  def genId(used: Set[String]) = {
    var suff: Int = 0
    var name = "x" + suff
    while (used(name)) {
      suff += 1
      name = "x" + suff
    }
    name
  }

  /** Finds the set of free variables in a lambda calculus expression. */
  def findFVs(exp: LExp,
                      fvs: Set[String] = Set.empty,
                      cvs: Set[String] = Set.empty): Set[String] = {
    exp match {
      case LVar(x) => if (cvs(x)) Set.empty else fvs + x
      case LLam(x, e) => findFVs(e, fvs, cvs + x)
      case LApp(e1, e2) => findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs)
    }
  }

  /** Replaces all instances of x in e1 with e2. */
  def subst(e1: LExp, x: String, e2: LExp): LExp = {
    e1 match {
      case LVar(s) => if (x == s) e2 else e1
      case LApp(t1, t2) => LApp(subst(t1, x, e2), subst(t2, x, e2))
      case LLam(s, e) =>
        if (s == x) 
          e2
        else  {
          val e2FVs = findFVs(e2)
          if (!e2FVs(s)) 
            LLam(s, subst(e, x, e2))
          else {
            val eFVs = findFVs(e)
            val usedVars = (eFVs union e2FVs) + s
            val freshVar = genId(usedVars)
            LLam(freshVar, subst(subst(e, s, LVar(freshVar)), x, e2))
          }
        }
    }
  }

  /** Takes one small step according to the lambda calc semantics */
  def reduce(exp: LExp): Option[Lexp] = {
    exp match {
      case LVar(_) => None
      case LLam(x, e) => {
        reduce(e) match {
          case Some(e1) => Some(LLam(x, e1))
          case None => None
        }
      }
      case LApp(e1, e2) => {
        e1 match {
          case LLam(x, t1) => Some(subst(t1, x, e2))
          case _ => {
            reduce(e1) match {
              case Some(e1r) => Some(LApp(e1r, e2))
              case None => {
                reduce(e2) match {
                  case Some(e2r) => Some(LApp(e1, e2r))
                  case None => None
                }
              }
            }
          }
        }
      }
    }
  }
}