package scalambda

import scala.io.StdIn

object LambdaCalcConsole {

  /** Message string constants */
  val header = """Scalambda REPL – Version 0.1 – Jacob Lurye
==========================================
  """

  val help = """available commands:
  :help           display this help message
  :load [file]    import identifier definitions from file
  :quit           exit REPL"""
  /** End message constants */

  val interpreter = new LambdaCalcInterpreter

  val navCmds = Set("\033A", "\033B", "\033C", "\033D")

  def readAndEvalLine: Unit = {
    print("\u03BB > ")
    StdIn.readLine() match {
      case ":quit" => ()
      case ":load" => println("LOADING!"); readAndEvalLine
      case ":help" => println(help); readAndEvalLine
      case cmd => println("=> " + interpreter.eval(cmd)); readAndEvalLine
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