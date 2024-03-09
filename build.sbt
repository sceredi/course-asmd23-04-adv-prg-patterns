ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.0"

lazy val root = (project in file("."))
  .settings(
    name := "course-asmd23-04-adv-prg-patterns",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
    ),
    Compile / scalaSource := baseDirectory.value / "src" / "main",
    Test / scalaSource := baseDirectory.value / "src" / "test",
  )
