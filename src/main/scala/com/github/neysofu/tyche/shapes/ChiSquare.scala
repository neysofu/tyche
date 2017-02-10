package com.github.neysofu
package tyche
package shapes

/** Represents a chi-squared distribution with ''k'' degrees of freedom.
 *
 *  @param k the ''k'' parameter of the distribution.
 */
case class ChiSquare(k: Int) extends ContinuousAutomaton {
    require(k >= 0)

  def apply: Double = {
    var sum = 0.0
    for (n <- 1 to k)
      sum += Math.pow(ChiSquare.stdNormal(), 2)
    sum
  }

  override def mean(implicit toDouble: Double => Double): Double = k

  override def stdDeviation(implicit toDouble: Double => Double): Double =
    Math.sqrt(2*k)
}

object ChiSquare {
  lazy val stdNormal = Gauss(0, 1)
}
