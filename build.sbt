// Site building with ´sbt-site´
enablePlugins(SiteScaladocPlugin)

val TycheLibraryVersion = "0.4.2-SNAPSHOT"

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

def travisSettings = sys.env.get("TRAVIS") match {
  case None => Seq()
  case Some(_) => Seq(
    // Signature
    pgpSecretRing := file("keys/secring.asc"),
    pgpPublicRing := file("keys/pubring.asc"),
    // Passphrase
    pgpPassphrase := Option(sys.env("SONATYPE_PGP").toArray),
    // Username/Password
    credentials += Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      sys.env("SONATYPE_USERNAME"),
      sys.env("SONATYPE_PASSWORD")
    )
  )
}

def dependencySettings = Seq(
  libraryDependencies ++= Seq(Dependencies.scalatest)
)

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
  settings = globalSettings ++ dependencySettings ++ publishSettings ++
    travisSettings
)
