import AssemblyKeys._

organization := "co.torri"

name := "reindxr"

version := "0.5"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "3.3.0",
  "org.apache.lucene" % "lucene-highlighter" % "3.3.0",
  "org.apache.lucene" % "lucene-analyzers" % "3.3.0",
  "org.apache.tika" % "tika-parsers" % "1.1",
  "com.typesafe.akka" % "akka-remote" % "2.0-RC1",
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "ch.qos.logback" % "logback-classic" % "0.9.29",
  "org.clapper" %% "grizzled-slf4j" % "0.6.6",
  "org.streum" % "configrity_2.9.0" % "0.7.0",
  "org.apache.yoko" % "yoko-spec-corba" % "1.3",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.3",
  "net.databinder" %% "dispatch-nio" % "0.8.5",
  "org.clapper" %% "avsl" % "0.3.6"
)

resolvers ++= Seq(
  "java m2" at "http://download.java.net/maven/2",
  "jboss repo" at "http://repository.jboss.org/nexus/content/groups/public-jboss/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Akka Maven2 Repository" at "http://akka.io/repository/",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "GuiceyFruit Release Repository" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

unmanagedBase <<= baseDirectory { base => base / "custom_lib" }

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
