import sbt._, Keys._

object Dependencies {

  object Resolvers {
   // val artima = "Artima Maven Repository" at "http://repo.artima.com/releases"
  
    val commons = Seq(
      //artima
    )
  }

  val scalactic = "org.scalactic" %% "scalactic" % "3.0.0"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.0" % "test"
}
