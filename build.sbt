import AssemblyKeys._

organization := "co.torri"

name := "reindxr"

version := "0.6.3"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "4.3.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.3.1",
  "org.apache.tika" % "tika-parsers" % "1.4",
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "org.clapper" %% "grizzled-slf4j" % "1.0.1",
  "org.streum" %% "configrity-core" % "1.0.0",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.8",
  "net.databinder" %% "dispatch-nio" % "0.8.10",
  "org.clapper" %% "avsl" % "1.0.1",
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
