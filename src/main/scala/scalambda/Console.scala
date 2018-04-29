package scalambda

import jacoblurye.repl._
import scala.io.StdIn
import scala.collection.immutable.ListMap

object LambdaCalcConsole {

  val loader = new LibParser
  val interpreter = new LambdaCalcInterpreter

  var definitionMap = ListMap[String, LExp]()

  val eval: Evaluator = (cmd: String) => {
    if ((cmd contains "=") && !(cmd contains "let")) {
      definitionMap += loader.parse(loader.defn, cmd).get
      ""
    } else {
      try {
        interpreter.eval(cmd, definitionMap)
      } catch {
        case e: Throwable => e.toString
      }
    }
  }

  val load: Command = (args: List[String]) => {
    for (fname <- args) {
      println("Loading definitons from " + fname) 
      definitionMap ++= loader.parseFile(fname)
    }
  }

  val repl = new REPL(eval, "\u03BB > ")
  repl.addCmd("load", load, "import identifier definitions from file")

  val header = """Scalambda REPL – Version 0.1 – Jacob Lurye
==========================================="""

  def run: Unit = {
    println(header)
    repl.run
  } 
}

object Main extends App {
  LambdaCalcConsole.run
}