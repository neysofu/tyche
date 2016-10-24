package io.neysofu.tyche
package stats

case class Binomial(n: Int, p: Double) extends DistributionWithMoments[Double] { self =>

  def get: Double = Seq.fill(n)(rnd.nextDouble).count(_ < p)
}
