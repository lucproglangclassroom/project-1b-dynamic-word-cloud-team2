name := "hello-scalatest-scala"

version := "0.3"

scalaVersion := "3.3.3"

scalacOptions += "@.scalacOptions.txt"

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.19"  % Test,
  "org.scalacheck" %% "scalacheck" % "1.18.0" % Test,
  "com.github.scopt" %% "scopt" % "4.1.0",
  "ch.qos.logback" % "logback-classic" % "1.4.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "org.openjfx" % "javafx-base" % "17" exclude("org.openjfx", "javafx-graphics"),
  "org.openjfx" % "javafx-graphics" % "17",
  "org.openjfx" % "javafx-controls" % "17",
  "org.openjfx" % "javafx-fxml" % "17"
)

enablePlugins(JavaAppPackaging)
