package io.neysofu.tyche

/** Defines a probability mass function and the associated cumulative density
 *  function for discrete random variables.
 */
trait MassFunction[A] {

  type MassMap[A] = Map[A, Double]

  /** The probability mass function. It contains all the possible outcomes
   *  and their respective weights. The weights are supposed to:
   *   1. be nonnegative, and
   *   2. sum up to 1.
   *
   *  @example {{{
   *  scala> val coinToss = new MassFunction[Boolean] {
   *       |   val mass = Map(true -> 0.5, false -> 0.5)
   *       | }
   *  coinToss: io.neysofu.tyche.MassFunction[Boolean] = \$...
   *
   *  }}}
   */
  val mass: MassMap[A]

  protected lazy val outcomes = mass.unzip._1.toList
  protected lazy val cdf = mass.unzip._2.scanLeft(0.0)(_+_).tail.toList
}
