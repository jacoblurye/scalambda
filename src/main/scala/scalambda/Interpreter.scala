package scalambda

/**
  * Lambda calculus interpreter that accommodates symbol definitions.
  */
object Interpreter {

  /** Parse a string lambda calculus expression and evaluate it to its normal form.
    *
    * @param s a string lambda calculus expression.
    * @param defs a sequence of identifiers and associated definitions used in `s`.
    * @return a string representation of the normal form of `s`.
    */
  def eval(s: String, defs: Seq[(String, Exp)] = Seq.empty): String = {
    LambdaCalcParser.parse(s) match {
      case None => "Error: could not parse expression " + s
      case Some(e) =>
        val res = e.withDefinitions(defs).normalForm()
        LambdaCalcParser.reverseParse(toId(res, defs.map(_.swap).toMap))
    }
  }

  // Replace occurrences of definitions with their identifiers
  private[scalambda] def toId(exp: Exp, defs: Map[Exp, String]): Exp = {
    if (defs.keySet(exp)) Var(defs(exp))
    else
      exp match {
        case Lam(x, e)   => Lam(x, toId(e, defs))
        case App(e1, e2) => App(toId(e1, defs), toId(e2, defs))
        case _           => exp
      }
  }
}
