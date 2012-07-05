import AssemblyKeys._

organization := "co.torri"

name := "reindxr"

version := "0.6.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "3.6.0",
  "org.apache.lucene" % "lucene-highlighter" % "3.6.0",
  "org.apache.lucene" % "lucene-analyzers" % "3.6.0",
  "org.apache.tika" % "tika-parsers" % "1.1",
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "org.clapper" %% "grizzled-slf4j" % "0.6.6",
  "org.streum" % "configrity_2.9.0" % "0.7.0",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.3",
  "net.databinder" %% "dispatch-nio" % "0.8.5",
  "org.clapper" %% "avsl" % "0.3.6",
  "co.torri" %% "scala-jsonr" % "0.5"
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
