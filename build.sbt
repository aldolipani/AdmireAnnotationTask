import sbt.Keys._

name := """AnnotationTask"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  javaEbean,
  anorm,
  cache,
  "org.mindrot" % "jbcrypt" % "0.3m",
  filters,
  "mysql" % "mysql-connector-java" % "5.1.18",
  "javax.persistence" % "persistence-api" % "1.0.2",
  "org.apache.lucene" % "lucene-core" % "4.6.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.6.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.6.1"
)

resolvers ++= Seq(
  "Apache" at "http://repo1.maven.org/maven2/",
  "jBCrypt Repository" at "http://repo1.maven.org/maven2/org/",
  "Sonatype OSS Snasphots" at "http://oss.sonatype.org/content/repositories/snapshots"
)

