package com.github.neysofu
package tyche
package generators

import scala.util.Random

case class MassFunction[A](mass: Map[A, Double]) extends MassTraitFunction[A] {
  require(!mass.isEmpty, "The PMF cannot be empty.")
  require(mass.values forall (_ >= 0), "All probabilities must be nonnegative.")
}

trait MassTraitFunction[A] extends (() => A) {
    val mass: Map[A, Double]

  /** The probability mass function.
   *
   *  It contains all the possible elements and their respective relative
   *  weights, which must be positive.
   */

  protected[this] lazy val values = mass.keys.toVector
  protected[this] lazy val weights =
    mass.values.toVector.scanLeft(0.0)(_+_).tail
  
  def apply: A = {
    val d = Random.nextDouble * weights.last
    values(weights indexWhere (_ > d))
  }
}
