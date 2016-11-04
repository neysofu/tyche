import sbt._, Keys._
import Dependencies._

val TycheLibraryVersion = "0.1.0"

lazy val root = (project in file("."))
  .settings(Seq(
    // Commons
    organization := "io.neysofu",
    name := "tyche",
    version := TycheLibraryVersion,
    // Compiler settings
    scalaVersion := "2.11.8",
    scalacOptions := Seq(
      "-deprecation",
      "-unchecked",
      "-feature"
    ),
    // Dependencies
    resolvers ++= Dependencies.Resolvers.commons,
    libraryDependencies ++= Seq(
      scalactic,
      scalatest
    )
  ))
