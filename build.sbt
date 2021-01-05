ThisBuild / scalaVersion := "2.12.12"
ThisBuild / organization := "advent2016"

lazy val hello = (project in file("."))
  .settings(
    name := "Advent2016",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
    libraryDependencies += "scala-advent-utils" %% "scalaadventutils" % "0.1.0-SNAPSHOT",
  )
