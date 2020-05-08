val common = Seq(
  scalaVersion := "2.12.8",
  organization := "csw"
)

lazy val core = project.in(file("core"))
  .settings(
    common: _*
  )
  .settings(
    name := "fparser",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
    )
  )

lazy val examples = project.in(file("examples"))
  .settings(
    common: _*
  )
  .dependsOn(core)