import sbt._
import sbt.Keys._

import spray.revolver.RevolverPlugin._

object Build extends Build {

  lazy val root = (project in file("."))
    .settings(Revolver.settings: _*)
    .settings(
      name                  := "server",
      organization          := "com.cotitan",
      version               := "0.1.0",
      scalaVersion          := "2.11.4",
      licenses              += ("MIT", url("http://opensource.org/licenses/MIT")),
      scalacOptions         += "-feature",
      scalacOptions         += "-deprecation",
      scalacOptions         += "-unchecked",
      scalacOptions         += "-language:implicitConversions",
      resolvers             += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies   += "com.typesafe.akka" % "akka-actor" % "2.0"
    )

}

