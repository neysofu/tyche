package io.neysofu.tyche

import io.neysofu.tyche.util.Util
import scala.util.Random

case class ContinuousDistribution(f: () => Double) extends ContinuousGen[Double] {
  def get = f()
}

object ContinuousDistribution {
 
  /** Returns a normal distribution.
   *
   *  @param sd the standard deviation
   *  @param eg the expected value
   */
  def normal(sd: Double, eg: Double): ContinuousDistribution = {
    new ContinuousDistribution(() => Random.nextGaussian * sd + eg)
  }

  /** Returns a chi-squared distribution.
   *
   *  @param k the degrees of freedom
   */
  def chiSquare(k: Int): ContinuousDistribution = {
    new ContinuousDistribution(
      () => Seq.fill(k)(Util.square(normal(1, 0).get)).sum)
  }

  /** Returns a binomial distribution.
   *
   *  @param n the number of independent yes/no experiments
   *  @param p the success probability
   */
  def binomial(n: Int, p: Double): ContinuousDistribution = {
    new ContinuousDistribution(
      () => Seq.fill(n)(Random.nextDouble).count(_ < p).toDouble)
  }

  /** Returns a uniform distribution in the interval `[0;1[`.
   */
  def uniform: ContinuousDistribution = new ContinuousDistribution(
    () => Random.nextDouble
  )
}
