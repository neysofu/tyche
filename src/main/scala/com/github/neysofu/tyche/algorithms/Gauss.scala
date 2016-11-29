package com.github.neysofu
package tyche
package algorithms

import scala.util.Random

case class Gauss(ev: Double, sd: Double) extends AbstractGen[Double] {

  def apply: Double = Random.nextGaussian * sd + ev

  override def mean(implicit toDouble: Double => Double): Double = ev
  override def stdDeviation(implicit toDouble: Double => Double): Double = sd
}
