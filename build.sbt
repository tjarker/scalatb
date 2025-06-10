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

// Add your published framework dependency
//libraryDependencies += "example" %% "mytest-framework" % "0.1.0"
libraryDependencies += "net.java.dev.jna" % "jna" % "5.13.0"
//libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.0"

// scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test


val chiselVersion = "6.0.0"
addCompilerPlugin(
  "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

// Tell sbt to use your test framework (class name from your jar manifest)
//testFrameworks += new TestFramework("mytest.MyTestFramework")
