import sbt._

lazy val commonSettings = Seq(
	organization := "io.neysofu",
	version := "0.0.1",
	scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
	settings(commonSettings: _*).
	settings(
		name := "Tyche",
		scalacOptions += "-feature",
        scalacOptions += "-deprecation",
        libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
	)




