name := "ConwayScalaJS"
version := "1.0"
scalaVersion := "2.13.3"

enablePlugins(ScalaJSPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
Compile / mainClass := Some("com.awwsmm.conway.Main") // start with the `main` in Main

// DOM -- npm install
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

// ScalaTags -- depends on scalajs-dom
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.2"

// Akka Actors
libraryDependencies += "org.akka-js" %%% "akkajsactortyped" % "2.2.6.14"