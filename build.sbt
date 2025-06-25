scalaVersion := "2.13.12"

name := "scalatb"
version := "0.0.1"

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  "-Ymacro-annotations"
)

fork := true

// Add your published framework dependency
//libraryDependencies += "example" %% "mytest-framework" % "0.1.0"
libraryDependencies += "net.java.dev.jna" % "jna" % "5.13.0"
//libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.0"

// scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.12"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.12.0",
  "io.circe" %% "circe-generic" % "0.12.0",
  "io.circe" %% "circe-parser" % "0.12.0"
)

val chiselVersion = "6.0.0"
addCompilerPlugin(
  "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

// Tell sbt to use your test framework (class name from your jar manifest)
//testFrameworks += new TestFramework("mytest.MyTestFramework")
