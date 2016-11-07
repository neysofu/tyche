package io.neysofu.tyche

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
    val nth = 1.0 / sampleSize
    toGenDouble.map(x => x * nth).take(sampleSize).sum
  }

  def standardDeviation(implicit toDouble: A <:< Double) = {
    val m = mean
    val nth = 1.0 / sampleSize
    map(x => (x*x - m) * nth).take(sampleSize).sum
  }
}
