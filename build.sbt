name := "matrix"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"se.scalablesolutions.akka" % "akka-actor" % "1.2",
  "org.apache.commons" % "commons-math" % "2.2",
//	"se.scalablesolutions.akka" % "akka-typed-actor" % "1.2",
//	"se.scalablesolutions.akka" % "akka-remote" % "1.2",
	"org.scalatest" %% "scalatest" % "1.6.1" % "test"
)

parallelExecution in IntegrationTest := false