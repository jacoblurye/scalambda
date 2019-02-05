package scalambda

/** A lambda calculus expression */
sealed trait Exp {

  /** Syntax string representation */
  def repr: String

  /** Find the free variables in this expression.
    *
    * @param bound a [[Set]] of bound variable identifiers.
    * @return the set of free variables in this expression.
    * */
  def free(bound: Set[String] = Set.empty): Set[String]

  /** Substitute all occurrences of a variable with an expression.
    *
    * @param subId identifier to replace with subExp`.
    * @param subExp expression to replace `subId` with.
    * @return this expression with `subExp` substituted for `subId`.
    */
  def substitute(subId: String, subExp: Exp): Exp

  /** Take one simplifying step */
  def reduce: Option[Exp]

  /** Attempt to fully reduce this expression to its most simplified form */
  def normalForm(maxIter: Int = 1000, debug: Boolean = false): Exp = {
    @scala.annotation.tailrec
    def _normalForm(exp: Exp, iter: Int = 0): Exp = {
      if (debug) println(exp)
      exp.reduce match {
        case Some(exp1) =>
          if (iter == maxIter) exp1 else _normalForm(exp1, iter + 1)
        case None => exp
      }
    }
    _normalForm(this)
  }

  /** Wrap this expression in identifier definitions */
  private[scalambda] def withDefinitions(
      definitions: Seq[(String, Exp)]): Exp = {
    definitions.foldLeft(this) {
      case (exp, (id, definition)) =>
        // Only wrap the expression with definitions it references
        if (exp.free().contains(id)) App(Lam(id, exp), definition)
        else exp
    }
  }
}

/** A variable.
  *
  * @param id string identifier associated with this variable.
  */
case class Var(id: String) extends Exp {
  def repr: String = id

  // A variable is free if it is not bound to an expression
  // in its enclosing scope.
  def free(bound: Set[String]): Set[String] =
    if (bound.contains(id)) Set.empty else Set(id)

  def substitute(x: String, subExp: Exp): Exp = if (x == id) subExp else this

  def reduce: Option[Exp] = None
}

/** A lambda function.
  *
  * @param id string identifier for the argument to this function.
  * @param body expression to be evaluated when the function is applied.
  */
case class Lam(id: String, body: Exp) extends Exp {
  def repr: String = s"/$id.${body.repr}"

  // In body, id is bound to the expression to which
  // this function is applied.
  def free(bound: Set[String]): Set[String] =
    body.free(bound + id)

  // We can only safely substitute subExp for id in body if
  // id is not a free variable of subExp. If id is in
  // subExp's free variables, we rewrite this function
  // in terms of a fresh identifier to avoid unintended
  // behavior upon function application.
  def substitute(subId: String, subExp: Exp): Exp =
    if (subId == id) this
    else if (!subExp.free().contains(id))
      Lam(id, body.substitute(subId, subExp))
    else {
      val freshId = Lam.nextFreshId
      Lam(freshId, body.substitute(id, Var(freshId)).substitute(subId, subExp))
    }

  def reduce: Option[Exp] = body.reduce.map(Lam(id, _))
}
object Lam {
  // Helper for generating unused identifiers. Its state
  // must be shared across Lam instances to avoid name collisions.
  private var nextId = -1
  def nextFreshId: String = {
    nextId += 1
    "$x" + nextId
  }
}

/** An expression application.
  *
  * @param e1 applicator.
  * @param e2 applicatee.
  */
case class App(e1: Exp, e2: Exp) extends Exp {
  def repr: String = s"(${e1.repr} ${e2.repr})"

  def free(bound: Set[String]): Set[String] =
    e1.free(bound) union e2.free(bound)

  def substitute(subId: String, subExp: Exp): Exp =
    App(e1.substitute(subId, subExp), e2.substitute(subId, subExp))

  def reduce: Option[Exp] = this match {
    case App(Lam(id, body), right) =>
      // Apply a function by replacing occurrences
      // of id in its body with its argument.
      Some(body.substitute(id, right))
    case _ =>
      // Reduce the left side then the right side.
      e1.reduce match {
        case None            => e2.reduce.map(App(e1, _))
        case Some(e1Reduced) => Some(App(e1Reduced, e2))
      }
  }
}

/** A let-expression, e.g. "let x = 1 in ((/y. y) x)".
  *
  * @param id the identifier being bound to `e1`.
  * @param e1 the expression definition of `id`.
  * @param e2 the expression in which to substitute `e1` for `id`.
  */
case class Let(id: String, e1: Exp, e2: Exp) extends Exp {
  def repr: String = s"let $id = ${e1.repr} in ${e2.repr}"

  // Let-statements are just syntactic sugar for function application.
  private val rep = App(Lam(id, e2), e1)

  def free(bound: Set[String]): Set[String] = rep.free(bound)

  def substitute(subId: String, subExp: Exp): Exp =
    rep.substitute(subId, subExp)

  def reduce: Option[Exp] = rep.reduce
}
