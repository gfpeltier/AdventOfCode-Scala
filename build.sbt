

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode",
    version := "0.1",
    scalaVersion := "2.13.4",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.1.0",
      "co.fs2" %% "fs2-io" % "3.1.0",
    )
  )
