package scalambda

import scala.io.StdIn

object LambdaCalcConsole {

  val interpreter = new LambdaCalcInterpreter

  val navCmds = Set("\033A", "\033B", "\033C", "\033D")

  def run: Unit = {
    print("\u03BB > ")
    StdIn.readLine() match {
      case ":quit" => ()
      case ":load" => println("LOADING!"); run
      case cmd => interpreter.eval(cmd); run
    }
    
  }

}

object Main extends App {
  LambdaCalcConsole.run
}