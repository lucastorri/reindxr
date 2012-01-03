import AssemblyKeys._

organization := "co.torri"

name := "reindxr"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "3.3.0",
  "org.apache.lucene" % "lucene-highlighter" % "3.3.0",
  "se.scalablesolutions.akka" % "akka-actor" % "1.3-RC2",
  "se.scalablesolutions.akka" % "akka-remote" % "1.3-RC2",
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "ch.qos.logback" % "logback-classic" % "0.9.29",
  "org.clapper" %% "grizzled-slf4j" % "0.6.6",
  "org.streum" % "configrity_2.9.0" % "0.7.0",
  "org.apache.yoko" % "yoko-spec-corba" % "1.3"
)

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Akka Maven2 Repository" at "http://akka.io/repository/"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

resolvers += "GuiceyFruit Release Repository" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"

seq(assemblySettings: _*)

scalacOptions += "-deprecation"
