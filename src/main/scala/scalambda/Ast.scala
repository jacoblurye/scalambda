package scalambda

sealed trait Exp extends Product with Serializable

case class Var(id: String) extends Exp
case class Lam(id: String, e: Exp) extends Exp
case class Let(id: String, e1: Exp, e2: Exp) extends Exp
case class LApp(e1: Exp, e2: Exp) extends Exp
