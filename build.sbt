import sbt._, Keys._
import Dependencies._

lazy val root = (project in file("."))
  .settings(Seq(
    // Commons
    organization := "io.neysofu",
    name := "tyche",
    version := "0.0.1",
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
