package io.neysofu.tyche
package stats

trait Discrete[A] {

  /** The probability mass function; it contains all the possible outcomes and
   *  the respective weights. The weights are supposed to:
   *   1. be nonnegative, and
   *   2. sum up to 1.
   */
  val mass: Seq[(Double, A)]

  protected lazy val cdf = mass.map(_._1).scanLeft(0.0)(_+_).tail
}
