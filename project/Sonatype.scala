import sbt._
import Keys._

object Sonatype {

  def username = Option(System.getenv().get("SONATYPE_USERNAME"))
  def password = Option(System.getenv().get("SONATYPE_PASSWORD"))
}

