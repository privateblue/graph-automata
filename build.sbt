val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graph-automata",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.scalatest" %% "scalatest" % "3.2.12" % "test",
      "org.scalatest" %% "scalatest-flatspec" % "3.2.12" % "test"
    )
  )
