ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "Breaker"
  )

libraryDependencies += "org.scalafx" % "scalafx_3" % "22.0.0-R33"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.3"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.2.0"

//kappale OS2 15.3
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-yaml"
).map(_ % circeVersion)