package com.github.neysofu
package tyche
package shapes

import scala.util.Random

/** A template for normal (Gaussian) distributions.
 *
 *  @param ev the ''μ'' parameter of the distribution.
 *  @param sd the ''σ²'' parameter of the distribution.
 */ 
case class Gauss(ev: Double, sd: Double) extends ContinuousAutomaton {

  def apply: Double = Random.nextGaussian * sd + ev

  override def mean(implicit toDouble: Double => Double): Double = ev
  override def stdDeviation(implicit toDouble: Double => Double): Double = sd
}
