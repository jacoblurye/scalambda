package scalambda

import scala.io.StdIn.readLine

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
      definitions ++= LambdaCalcParser.loadDefinitionsFile(path)
    })

  private val quit: CommandFunc = _ => {
    println("bye!")
    sys.exit(0)
  }

  private val help: CommandFunc = _ => {
    commands.foreach {
      case (name, Command(_, helpStr)) =>
        println(s":$name\t\t$helpStr")
    }
  }

  private val commands: Map[String, Command] = Map[String, Command](
    "load" -> Command(load, "import identifier definitions from file"),
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
  lazy val run: Unit = {
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
