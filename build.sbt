import scala.sys.process._

name := "scalambda"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

lazy val install = taskKey[Unit]("Install the scalambda REPL")

install := {
  // Don't run until project is packaged and published locally
  publishLocal.value

  // Run installation script
  "chmod u+x ./project/install.sh".!
  "./project/install.sh".!
}
