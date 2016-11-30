package com.github.neysofu
package tyche
package algorithms

import scala.util.Random

case class Uniform(a: Double, b: Double) extends Gen[Double] {
  require(a <= b, "𝑎 shall be greater or equal to 𝑏.")

  def apply: Double = Random.nextDouble * (a-b) + b

  override def mean(implicit toDouble: Double => Double): Double = (a+b) / 2

  override def stdDeviation(implicit toDouble: Double => Double): Double =
    (b - a) / Math.sqrt(12)
}
