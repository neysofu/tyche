import io.neysofu.tyche.DiscreteDistribution

object FamilyExample {

  sealed trait Child
  object Boy extends Child
  object Girl extends Child

  val family = DiscreteDistribution.uniform(Boy, Girl)
    .until(_ contains Boy)
    .map(_.size)

  def main(args: Array[String]): Unit = {
    println("The average family size is %s",
      family.take(sampleSize).sum / sampleSize
    )
  }
}
