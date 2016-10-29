package io.neysofu.tyche
package stats

/** Represents a discrete probability distribution.
 *
 *  Resources
 *  ---------
 *  1. https://www.youtube.com/watch?v=3MOahpLxj6A
 */
trait DiscreteGen[A] extends MomentsGen[A] with Gen[A] { self =>

  /** The probability mass function of this probability distribution. It is
   *  assumed that:
   *   1. the sum of the weights is 1;
   *   2. each and every weight is nonnegative.
   */
  val pmf: Seq[(Double, A)]

  override def get: A = {
    val d = rnd.nextDouble
    pmf(cumulativeWeights.indexWhere(_ > d))._2
  }

  override def mean(implicit toDouble: A <:< Double): Double = {
    pmf.map(p => p._1 * toDouble(p._2)).sum / pmf.length
  }

  override def stdDeviation(implicit toDouble: A <:< Double): Double = {
    // http://www.stat.yale.edu/Courses/1997-98/101/rvmnvar.htm
    val m = mean
    pmf.map(p => Math.pow(toDouble(p._2) * m, 2) * p._1).sum / pmf.length
  }

  private lazy val cumulativeWeights: Seq[Double] = {
    var t = 0.0
    pmf.map { p =>
      t += p._1
      t
    }
  }
}
