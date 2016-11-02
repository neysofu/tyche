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
   *  val mass = Seq((-1.0, true), (2.0, false))
   *
   *  // Right
   *  val mass = Seq((0.5, true), (0.5, false))
   *  }}}
   */
  val mass: Seq[(Double, A)]
  protected lazy val cdf = mass.unzip._1.scanLeft(0.0)(_+_).tail
}
