ThisBuild / version := "0.1.0-SNAPSHOT"

//ThisBuild / scalaVersion := "3.1.1"
ThisBuild / scalaVersion := "2.13.3"

libraryDependencies += "com.google.truth" % "truth" % "1.1.3" % Test
libraryDependencies += "org.parboiled" %% "parboiled" % "2.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "WhileLang",
    idePackagePrefix := Some("weigl")
  )
