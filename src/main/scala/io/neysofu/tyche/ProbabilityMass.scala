package io.neysofu.tyche

/**
 * Defines a discrete random variables through a probability mass function
 * (PMF) and the associated cumulative density function (CDF).
 */
trait ProbabilityMass[A] {

  /**
   * The probability mass function; it contains all the possible outcomes and
   * the respective weights. The weights are supposed to:
   *  1. be nonnegative, and
   *  2. sum up to 1.
   */
  val mass: Seq[(Double, A)]

  protected lazy val cdf = mass.map(_._1).scanLeft(0.0)(_+_).tail
}
