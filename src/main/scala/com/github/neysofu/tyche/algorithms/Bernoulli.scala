package com.github.neysofu
package tyche
package algorithms

import scala.util.Random

/** Builds a new Bernoulli distribution.
 *  
 *  @param p the 𝑃 parameter of the Bernoulli distribution.
 *  @return a new Bernoulli distribution with the given 𝑃 parameter `p`.
 */
case class Bernoulli(p: Double) extends DiscreteDistribution[Boolean] {

  val q: Double = 1 - p

  override def apply: Boolean = Random.nextDouble < p

  def mass: Map[Boolean, Double] = Map(true -> p, false -> q)
}
