import sbt._
import Keys._

object Sonatype {

  def username = Option(sys.env.get("SONATYPE_USERNAME"))
  def password = Option(sys.env.get("SONATYPE_PASSWORD"))
}

