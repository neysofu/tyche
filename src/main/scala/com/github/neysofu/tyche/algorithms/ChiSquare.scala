package com.github.neysofu
package tyche
package algorithms

/**
 * @param k the ''𝑘'' parameter of the returned distribution.
 */
case class ChiSquare(k: Int) extends Gen[Double] {

  def apply: Double = {
    var sum = 0.0
    for (n <- 1 to k) sum += Math.pow(ChiSquare.stdNormal(), 2)
    sum
  }

  override def mean(implicit toDouble: Double => Double): Double = k

  override def stdDeviation(implicit toDouble: Double => Double): Double =
    Math.sqrt(2*k)
}

object ChiSquare {
  lazy val stdNormal = Gauss(0, 1)
}
