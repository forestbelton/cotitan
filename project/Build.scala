import sbt._
import sbt.Keys._

import spray.revolver.RevolverPlugin._

object Build extends Build {

    lazy val root = (project in file("."))
        .settings(Revolver.settings: _*)
        .settings(
            name                := "server",
            organization        := "com.cotitan",
            version             := "0.1.0",
            scalaVersion        := "2.11.4",
            licenses            += ("MIT", url("http://opensource.org/licenses/MIT")),
            scalacOptions       += "-feature",
            scalacOptions       += "-deprecation",
            scalacOptions       += "-unchecked",
            scalacOptions       += "-language:implicitConversions",
            resolvers           += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
            resolvers           += "Plasma Conduit Repository" at "http://dl.bintray.com/plasmaconduit/releases",
            resolvers           += "Spray" at "http://repo.spray.io",
            libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9",
            libraryDependencies += "com.wandoulabs.akka" %% "spray-websocket" % "0.1.4",
            libraryDependencies += "com.plasmaconduit" %% "json" % "0.19.0"
        )

}

