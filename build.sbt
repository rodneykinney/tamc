
enablePlugins(ScalaJSPlugin)

name := "TicTacToe"

skip in packageJSDependencies := false

scalaVersion := "2.11.6" // or any other Scala version >= 2.10.2

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
