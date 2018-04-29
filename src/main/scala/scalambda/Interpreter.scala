package scalambda

import scala.collection.immutable.ListMap

/**
  * A normal-order interpreter for the lambda calculus.
  */
class LambdaCalcInterpreter {

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
      case LLam(s, e) if (s == x) => e1 
      case LLam(s, e) if (!findFVs(e2)(s)) => LLam(s, subst(e, x, e2))
      case LLam(s, e) =>
        val fvar = genId
        LLam(fvar, subst(subst(e, s, LVar(fvar)), x, e2))
    }
  }

  /** Takes one small step according to call-by-name lambda calc semantics */
  def reduce(exp: LExp): Option[LExp] = {
    exp match {
      case LVar(_) => None
      case LLam(x, t) => reduce(t).map(LLam(x, _))
      case LLet(x, e1, e2) => reduce(LApp(LLam(x, e2), e1))
      case LApp(LLam(x, t), e2) => Some(subst(t, x, e2))
      case LApp(e1, e2) => 
        val res1 = reduce(e1).map(LApp(_, e2))
        if (res1 != None) res1 
        else reduce(e2).map(LApp(e1,_))
    }
  }

  /** Reduce an expression to its normal form */
  @scala.annotation.tailrec
  var c = 0;
  def normal_form(exp: LExp): LExp = {
    // FOR DEBUGGING:
    // println("===============================")
    // println(parser.revparse(exp))
    reduce(exp) match {
      case Some(exp1) => c += 1; if (c==100) exp1 else normal_form(exp1)
      case None => exp
    }
  }

  val parser = new LambdaCalcParser

  /** Replace subexpressions that have a definition with their id */
  def toId(exp: LExp, defs: ListMap[LExp, String]): LExp = {
    if (defs.keySet(exp)) LVar(defs(exp))
    else exp match {
      case LLam(x, e) => LLam(x, toId(e, defs))
      case LApp(e1, e2) => LApp(toId(e1, defs), toId(e2, defs))
      case _ => exp
    }
  }

  /** Fully evaluate an input string */
  def eval(s: String, defs: ListMap[String, LExp] = ListMap()): String = {
    parser.parse(s) match {
        case None => "Error: could not parse expression " + s
        case Some(e) => 
          // Wrap e in required definitions
          val eWithDefs = defs.foldLeft(e) {
            case (exp, (x, t)) => 
              if (findFVs(exp)(x)) LApp(LLam(x, exp), t)
              else exp
          }
          val res = normal_form(eWithDefs)
          parser.revparse(toId(res, defs.map(_.swap)))
      }
    }
}