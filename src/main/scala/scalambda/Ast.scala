/*
 * Implements the abstract syntax tree of the lambda calculus:
 *   <exp> ::= <var> | \<var>.<exp> | <exp.1> <exp.2>
 */
package scalambda

sealed trait LExp extends Product with Serializable

case class LVar(id: String) extends LExp
case class LLam(id: String, e: LExp) extends LExp
case class LLet(id: String, e1: LExp, e2: LExp) extends LExp
case class LApp(e1: LExp, e2: LExp) extends LExp