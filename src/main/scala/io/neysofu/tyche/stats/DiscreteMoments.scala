package io.neysofu.tyche
package stats

trait DiscreteMoments[A] extends Discrete[A] with Moments[A] {

  def plot(x: Int, y: Int)(implicit toDouble: A <:< Double): String = ""

  def mean(implicit toDouble: A <:< Double): Double =
    mass.map(p => p._1 * toDouble(p._2)).sum / mass.size

  def stdDeviation(implicit toDouble: A <:< Double): Double = {
    val m = mean
    mass.map(p => Math.pow(toDouble(p._2) * m, 2) * p._1).sum / mass.size
  }
}
