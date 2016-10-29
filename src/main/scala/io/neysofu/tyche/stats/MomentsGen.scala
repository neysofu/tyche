package io.neysofu.tyche
package stats

trait MomentsGen[A] extends Gen[A] {

  /** Returns the expected value (mean) of the probability distribution.
   */
  def mean(implicit toDouble: A <:< Double): Double

  /** Returns the standard deviation of the probability distribution.
   */
  def stdDeviation(implicit toDouble: A <:< Double): Double

  /** Returns the variance of the probability distribution.
   */
  def variance(implicit toDouble: A <:< Double): Double = {
    Math.pow(variance, 2)
  }
}
