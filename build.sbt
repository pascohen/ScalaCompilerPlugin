name := "scalaCompilerPlugin"

version := "1.0"

scalaVersion := "2.11.0-M4"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

libraryDependencies += "org.scala-lang"%"scala-compiler"%"2.11.0-M4"

libraryDependencies += "turtledsl_scala"%%"turtledsl_scala"%"1.0"
