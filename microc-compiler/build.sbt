name := "microc-compiler"

version := "1.0"

scalaVersion := "2.12.12"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// PEG (Parsing Expression Grammars ) library (cf. https://github.com/sirthias/parboiled2)
libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.1"
// Testing framework (cf. https://github.com/lihaoyi/utest)
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

Test / parallelExecution := false

assembly / assemblyJarName := "microc-compiler.jar"
