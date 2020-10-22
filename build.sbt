name := "ConwayScalaJS"
version := "0.1"
scalaVersion := "2.13.3"

enablePlugins(ScalaJSPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
mainClass in Compile := Some("conway.Main") // start with the `main` in Main

// DOM -- npm install
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

// uTest
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

// ScalaTags -- depends on scalajs-dom
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.2"

// Akka Actors
libraryDependencies += "org.akka-js" %%% "akkajsactor" % "2.2.6.9"