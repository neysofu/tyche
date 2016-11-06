import sbt._, Keys._
import Dependencies._

val TycheLibraryVersion = "0.2.0"

val sharedSettings = Seq(
  organization := "io.neysofu",
  version := TycheLibraryVersion,
  scalaVersion := "2.12.0",
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  resolvers ++= Dependencies.Resolvers.commons,
  libraryDependencies ++= Seq(
    //scalactic,
    scalatest
  )
)

lazy val root = Project(
  id = "tyche",
  base = file("."),
  settings = sharedSettings
).settings(
  name := "tyche"  
)
