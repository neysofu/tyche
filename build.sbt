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
		scalaSource in Compile := baseDirectory.value / "src",
		scalaSource in Test := baseDirectory.value / "test",
		libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",
		resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
	)




