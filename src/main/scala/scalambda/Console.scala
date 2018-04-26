package scalambda

import scala.io.StdIn
import scala.collection.immutable.ListMap

object LambdaCalcConsole {

  /** Message string constants */
  val header = """Scalambda REPL – Version 0.1 – Jacob Lurye
=========================================="""

  val help = """available commands:
:help           display this help message
:load [file]    import identifier definitions from file
:quit           exit REPL"""
  /** End message constants */

  val navCmds = Set("\033A", "\033B", "\033C", "\033D")

  var definitionMap = ListMap[String, LExp]()
  val loader = new LibParser
  val interpreter = new LambdaCalcInterpreter

  def readAndEvalLine: Unit = {
    print("\u03BB > ")
    try {
      StdIn.readLine() match {
        case ":quit" => ()
        case ":help" => 
          println(help)
          readAndEvalLine
        case cmd if cmd.startsWith(":load ") => 
          val fname = cmd.slice(6, cmd.length)
          println("Loading definitons from " + fname) 
          definitionMap ++= loader.parseFile(fname)
          readAndEvalLine
        case cmd =>
          if (cmd contains "=") {
            definitionMap += loader.parse(loader.defn, cmd).get
          } else {
            val res = interpreter.eval(cmd, definitionMap)
            println("=> " + res)
          }
          readAndEvalLine
      }
    } catch {
      case e => println(e); readAndEvalLine
    }
  }

  def run: Unit = {
    println(header)
    readAndEvalLine
  } 

}

object Main extends App {
  LambdaCalcConsole.run
}