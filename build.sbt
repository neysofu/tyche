val TycheLibraryVersion = "0.3.1-SNAPSHOT"

def globalSettings = Seq(
  organization := "com.github.neysofu",
  version := TycheLibraryVersion,
  scalaVersion := "2.12.0",
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  )
)

def dependencySettings = Seq(
  libraryDependencies ++= Seq(Dependencies.scalatest)
)

def secreteSettings = {
  val username = Sonatype.username
  val password = Sonatype.password
  if (!(username.isEmpty || password.isEmpty)) Seq(
    credentials += Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      username.get,
      password.get)
  ) else Seq()
}

def publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <url>https://github.com/neysofu/tyche</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer><id>neysofu</id></developer>
    </developers>
  )
)

lazy val root = Project(
  id = "tyche",
  base = file("."),
  settings = globalSettings ++ dependencySettings ++ secreteSettings ++
    publishSettings
)
