scalaVersion := "0.12.0-RC1"

libraryDependencies +=
  ("org.scalatest" %% "scalatest" % "3.0.5" % "test")
    .withDottyCompat(scalaVersion.value)