name := "parser-combinator-tutorial"

version := "0.0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.3.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.parboiled" %% "parboiled" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "0.2.1",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)
