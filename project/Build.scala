import sbt._
import Keys._

object B extends Build
{
  lazy val root =
    Project("root", file("."))
      .configs( IntegrationTest )
      .settings( Defaults.itSettings : _*)
      .settings( libraryDependencies ++= specs )

//  lazy val specs = "org.scala-tools.testing" %% "specs" % "1.6.8" % "it,test"
  lazy val specs = 	Seq(
    "org.scalatest" %% "scalatest" % "1.6.1" % "it,test",
    "junit" % "junit" % "4.10" % "test"
  )
}
