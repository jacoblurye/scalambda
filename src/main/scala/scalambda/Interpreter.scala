package scalambda

/**
  * A normal-order interpreter for the lambda calculus.
  */
class LambdaCalcInterpreter extends LambdaCalcParser {

  /** Generates unused variable id */
  private var fresh_id = -1;
  def genId = {
    fresh_id += 1
    "$x" + fresh_id
  }

  /** Finds the set of free variables in a lambda calculus expression. */
  def findFVs(exp: LExp,
              fvs: Set[String] = Set.empty,
              cvs: Set[String] = Set.empty): Set[String] = {
    exp match {
      case LVar(x) => if (cvs(x)) Set.empty else fvs + x
      case LLam(x, e) => findFVs(e, fvs, cvs + x)
      case LApp(e1, e2) => findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs)
      case LLet(x, e1, e2) => findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs + x)
    }
  }

  /** Replaces all instances of x in e1 with e2. */
  def subst(e1: LExp, x: String, e2: LExp): LExp = {
    e1 match {
      case LVar(s) => if (x == s) e2 else e1
      case LApp(t1, t2) => LApp(subst(t1, x, e2), subst(t2, x, e2))
      case LLet(s, t1, t2) => subst(LApp(LLam(s, t2), t1), x, e2)
      case LLam(s, e) => if (s == x) e1 else {
        if (!findFVs(e2)(s)) {
          LLam(s, subst(e, x, e2))
        } else {
          val fvar = genId
          LLam(fvar, subst(subst(e, s, LVar(fvar)), x, e2))
        }
      }
    }
  }

  /** Takes one small step according to normal-order lambda calc semantics */
  def reduce(exp: LExp): Option[LExp] = {
    exp match {
      case LVar(_) => None
      case LLam(x, t) => reduce(t).map(LLam(x, _))
      case LLet(x, e1, e2) => reduce(LApp(LLam(x, e2), e1))
      case LApp(e1, e2) => e1 match {
        case LLam(x, t) => Some(subst(t, x, e2))
        case _ => reduce(e1) match {
          case Some(re1) => Some(LApp(re1, e2))
          case None => reduce(e2) match {
            case Some(re2) => Some(LApp(e1, re2))
            case None => None
          }
        }
      }
    }
  }

  /** Reduce an expression to its normal form */
  def normal_form(exp: LExp): LExp = {
    reduce(exp) match {
      case Some(exp1) => normal_form(exp1)
      case None => exp
    }
  }

  /** Fully evaluate an input string */
  def eval(s: String): String = {
    revparse(normal_form(parse(s)))
  }
}