package io.neysofu.tyche
package stats

/** Provides fundamental numerical analysis methods to some probability
 *  distribution.
 *
 *  Resources
 *  ---------
 *  1. https://en.wikipedia.org/wiki/Numerical_analysis
 */
abstract class ContinuousDistribution[A] extends Distribution[A] { self =>

  override def map[B](f: A => B) = new ContinuousDistribution[B] {
    def get = f(self.get)
  }

  /** The number of observations to include in statistical samples.
   */
  val sampleSize: Int = 10000

  /** Returns the expected value (mean) of the probability distribution.
   */
  def mean(implicit toDouble: A <:< Double): Double = {
    times(sampleSize).map(toDouble(_)).sum / sampleSize
  }

  /** Returns the variance of the probability distribution.
   */
  def variance(implicit toDouble: A <:< Double): Double = {
    val m = mean
    def diff(x: Double) = Math.pow(x, 2) - m
    map(diff(_)).mean
  }

  /** Returns the standard deviation of the probability function.
   */
  def stdDeviation(implicit toDouble: A <:< Double): Double = {
    Math.sqrt(variance)
  }

  def toDouble(implicit toDouble: A <:< Double): Distribution[Double] = map {
    toDouble(_)
  }
}
