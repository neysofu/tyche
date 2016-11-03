package io.neysofu.tyche

/** This trait defines a probability mass function and the associated
 *  cumulative density function for discrete random variables.
 */
trait MassFunction[A] {

  /** The probability mass function. It contains all the possible outcomes
   *  and the respective weights. The weights are supposed to:
   *   1. be nonnegative, and
   *   2. sum up to 1.
   *
   *  {{{
   *  // Wrong
   *  val mass = Map(true -> -1.0, false -> 2.0)
   *
   *  // Right
   *  val mass = Map(true -> 0.5, false -> 0.5)
   *  }}}
   */
  val mass: Map[A, Double]
  protected lazy val outcomes = mass.unzip._1.toList
  protected lazy val cdf = mass.unzip._2.scanLeft(0.0)(_+_).tail.toList
}
