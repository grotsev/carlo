enablePlugins(ScalaJSPlugin)

name := "carlo"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.10.6",
  "org.scala-lang" % "scala-reflect" % "2.10.6",
  "name.lakhin.eliah.projects.papacarlo" %% "papa-carlo" % "0.7.0"
)
