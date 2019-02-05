package scalambda

import java.io.FileNotFoundException

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object REPLRunner extends scala.App {
  REPL.run
}

object REPL {

  private type CommandFunc = Seq[String] => Unit
  private case class Command(f: CommandFunc, help: String)

  private var definitions = Seq.empty[(String, Exp)]
  private val load: CommandFunc = args =>
    args.foreach(path => {
      println(s"Loading definitions from $path")
      Try(LambdaCalcParser.loadDefinitionsFile(path)) match {
        case Success(definition) => definitions ++= definition
        case Failure(e)          => println(e)
      }
    })

  private val defs: CommandFunc = _ => {
    definitions.foreach {
      case (name, exp) => println(s"$name = ${exp.repr}")
    }
  }

  private val quit: CommandFunc = _ => {
    println("bye!")
    sys.exit(0)
  }

  private val help: CommandFunc = _ => {
    commands.foreach {
      case (name, Command(_, helpStr)) =>
        println(s":$name\t$helpStr")
    }
  }

  private val commands: Map[String, Command] = Map[String, Command](
    "load" -> Command(load, "import identifier definitions from file"),
    "defs" -> Command(defs, "list currently available identifier definitions"),
    "quit" -> Command(quit, "exit the repl"),
    "help" -> Command(help, "display this help message")
  )

  private def exec(name: String, args: Seq[String]): Unit = {
    commands.get(name) match {
      case Some(Command(command, _)) => command(args)
      case None                      => println(s":$name : command not found")
    }

  }

  /** Run the REPL */
  @scala.annotation.tailrec
  def run: Unit = {
    readLine("\u03BB > ") match {
      case cmdAndArgs if cmdAndArgs.startsWith(":") =>
        val splitCommand = cmdAndArgs.drop(1).split(" +")
        exec(splitCommand.head, splitCommand.tail)
      case expr =>
        val res = Interpreter.eval(expr, definitions)
        if (res != "") println(res)
    }
    run
  }
}
