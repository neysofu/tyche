package io.neysofu.tyche
package stats

/** Provides fundamental numerical analysis methods to some probability
 *  distribution.
 *
 *  Resources
 *  ---------
 *  1. https://en.wikipedia.org/wiki/Numerical_analysis
 */
trait SamplingGen[A] extends MomentsGen[A] with Gen[A] { self =>

  /** The number of observations to include in statistical samples.
   */
  val sampleSize: Int = 10000

  override def mean(implicit toDouble: A <:< Double) = {
    times(sampleSize).map(toDouble(_)).sum / sampleSize
  }

  override def stdDeviation(implicit toDouble: A <:< Double) = {
    def diff(x: Double, m: Double) = Math.pow(x, 2) - m
    map(diff(_, mean)).times(sampleSize).sum / sampleSize
  }
}
