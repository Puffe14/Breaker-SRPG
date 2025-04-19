ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "Breaker"
  )

libraryDependencies += "org.scalafx" % "scalafx_3" % "22.0.0-R33"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.3"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.2.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.16"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test"