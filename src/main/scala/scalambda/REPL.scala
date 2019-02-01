package scalambda

import scala.io.StdIn._

object REPL {
  private val definitionMap =
    scala.collection.mutable.Map.empty[String, Exp]

  private case class Command(f: Seq[String] => Unit, help: String)

  private val load = (args: Seq[String]) =>
    args.foreach(fname => {
      println("Loading definitons from " + fname)
      definitionMap ++= LambdaCalcParser.parseDefinitionFile(fname)
    })

  private val quit = (_: Seq[String]) => {
    println("bye!")
    sys.exit(0)
  }

  private val help = (_: Seq[String]) => {
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
        val res = Interpreter.eval(expr, definitionMap.toMap)
        if (res != "") println(res)
    }
    run
  }

}
