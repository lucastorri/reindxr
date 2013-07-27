import AssemblyKeys._

organization := "co.torri"

name := "reindxr"

version := "0.6.3"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "4.4.0",
  "org.apache.lucene" % "lucene-highlighter" % "4.4.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.4.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.4.0",
  "org.apache.tika" % "tika-parsers" % "1.4",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.8",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.slf4j" % "slf4j-jdk14" % "1.7.5",
  "org.json4s" %% "json4s-native" % "3.2.4"
)

seq(assemblySettings: _*)

scalacOptions += "-deprecation"

homepage := Some(url("https://github.com/lucastorri/scala-jsonr/"))

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

net.virtualvoid.sbt.graph.Plugin.graphSettings

