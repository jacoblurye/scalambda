package scalambda

import scala.collection.immutable.ListMap

/**
  * A normal-order interpreter for the lambda calculus.
  */
object Interpreter {

  // Generates unused variable id
  private var fresh_id = -1;
  private def genId = {
    fresh_id += 1
    "$x" + fresh_id
  }

  /** Finds the set of free variables in a lambda calculus expression. */
  def findFVs(exp: Exp,
              fvs: Set[String] = Set.empty,
              cvs: Set[String] = Set.empty): Set[String] = {
    exp match {
      case Var(x)       => if (cvs(x)) Set.empty else fvs + x
      case Lam(x, e)    => findFVs(e, fvs, cvs + x)
      case LApp(e1, e2) => findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs)
      case Let(x, e1, e2) =>
        findFVs(e1, fvs, cvs) union findFVs(e2, fvs, cvs + x)
    }
  }

  /** Replaces all instances of x in e1 with e2. */
  def subst(e1: Exp, x: String, e2: Exp): Exp = {
    e1 match {
      case Var(s)                       => if (x == s) e2 else e1
      case LApp(t1, t2)                 => LApp(subst(t1, x, e2), subst(t2, x, e2))
      case Let(s, t1, t2)               => subst(LApp(Lam(s, t2), t1), x, e2)
      case Lam(s, e) if s == x          => e1
      case Lam(s, e) if !findFVs(e2)(s) => Lam(s, subst(e, x, e2))
      case Lam(s, e) =>
        val fvar = genId
        Lam(fvar, subst(subst(e, s, Var(fvar)), x, e2))
    }
  }

  /** Takes one small step according to call-by-name lambda calc semantics */
  def reduce(exp: Exp): Option[Exp] = {
    exp match {
      case Var(_)              => None
      case Lam(x, t)           => reduce(t).map(Lam(x, _))
      case Let(x, e1, e2)      => reduce(LApp(Lam(x, e2), e1))
      case LApp(Lam(x, t), e2) => Some(subst(t, x, e2))
      case LApp(e1, e2) =>
        val res1 = reduce(e1).map(LApp(_, e2))
        if (res1.isDefined) res1
        else reduce(e2).map(LApp(e1, _))
    }
  }

  /** Reduce an expression to its normal form */
  @scala.annotation.tailrec
  var c = 0;
  def normal_form(exp: Exp): Exp = {
    reduce(exp) match {
      case Some(exp1) => c += 1; if (c == 100) exp1 else normal_form(exp1)
      case None       => exp
    }
  }

  /** Replace subexpressions that have a definition with their id */
  def toId(exp: Exp, defs: Map[Exp, String]): Exp = {
    if (defs.keySet(exp)) Var(defs(exp))
    else
      exp match {
        case Lam(x, e)    => Lam(x, toId(e, defs))
        case LApp(e1, e2) => LApp(toId(e1, defs), toId(e2, defs))
        case _            => exp
      }
  }

  /** Fully evaluate an input string */
  def eval(s: String, defs: Map[String, Exp] = Map.empty): String = {
    LambdaCalcParser.parse(s) match {
      case None    => "Error: could not parse expression " + s
      case Some(e) =>
        // Wrap e in required definitions
        val eWithDefs = defs.foldLeft(e) {
          case (exp, (x, t)) =>
            if (findFVs(exp)(x)) LApp(Lam(x, exp), t)
            else exp
        }
        val res = normal_form(eWithDefs)
        LambdaCalcParser.revparse(toId(res, defs.map(_.swap)))
    }
  }
}
