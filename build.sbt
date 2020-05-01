organization := "co.torri"

name := "reindxr"

version := "0.6.3"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "4.4.0",
  "org.apache.lucene" % "lucene-highlighter" % "4.4.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.4.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.4.0",
  "org.apache.tika" % "tika-parsers" % "1.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "org.http4s" %% "http4s-dsl" % "0.21.4",
  "org.http4s" %% "http4s-blaze-server" % "0.21.4",
  "org.http4s" %% "http4s-blaze-client" % "0.21.4",
  "org.http4s" %% "http4s-circe" % "0.21.4",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-literal" % "0.13.0",

  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)

scalacOptions ++= Seq(
  "-deprecation"
)