package io.neysofu.tyche

import io.neysofu.tyche.util.Util

/** This trait represents a continuous probability distribution with
 *  [[https://goo.gl/5xB1A4 simple random sampling]] facilities.
 */
trait ContinuousGen[A] extends Gen[A] with Sampling with Moments[A] { self =>

  def virtualPlot(implicit toDouble: A <:< Double): Map[A, Double] = {
    take(sampleSize)
    .sortBy(toDouble(_))
    .grouped(sampleSize / 12)
    .toList
    .map(seq => seq.last -> 1.0 * sampleSize / seq.size).toMap
  }

  def mean(implicit toDouble: A <:< Double) = {
    toGenDouble.map(x => x * nthSampleSize).take(sampleSize).sum
  }

  def standardDeviation(implicit toDouble: A <:< Double) = {
    val m = mean
    map(x => (x*x-m) * nthSampleSize).take(sampleSize).sum
  }
}
