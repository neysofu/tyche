import sbt._
import Keys._

object Sonatype {

  def username = sys.env.get("SONATYPE_USERNAME")
  def password = sys.env.get("SONATYPE_PASSWORD")
}

