package com.github.neysofu.tyche

import scala.util.Random

/** A wrapper class for [[com.github.neysofu.tyche.ContinuousGen]].
 */
case class ContinuousDistribution(f: () => Double) extends ContinuousGen[Double] {
  def get = f()
}

object ContinuousDistribution {
 
  /** Returns a normal distributcom.github..
   *
   *  @param sd the standard deviatcom.github.
   *  @param eg the expected value
   */
  def normal(sd: Double, eg: Double): ContinuousDistribution = {
    new ContinuousDistribution(() => Random.nextGaussian * sd + eg)
  }

  /** Returns a chi-squared distributcom.github..
   *
   *  @param k the degrees of freedom
   */
  def chiSquare(k: Int): ContinuousDistribution = {
    new ContinuousDistribution(
      () => Seq.fill(k)(util.square(normal(1, 0).get)).sum)
  }

  /** Returns a binomial distributcom.github..
   *
   *  @param n the number of independent yes/no experiments
   *  @param p the success probability
   */
  def binomial(n: Int, p: Double): ContinuousDistribution = {
    new ContinuousDistribution(
      () => Seq.fill(n)(Random.nextDouble).count(_ < p).toDouble)
  }

  /** Returns a uniform distributcom.github. in the interval `[0;1[`.
   */
  def uniform: ContinuousDistribution = new ContinuousDistribution(
    () => Random.nextDouble
  )
}
