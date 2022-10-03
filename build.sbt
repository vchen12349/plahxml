  ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.8"

lazy val root = (project in file("."))
  .settings(
    name := "scalatest",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
  )
