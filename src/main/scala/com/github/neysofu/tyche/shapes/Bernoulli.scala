package com.github.neysofu
package tyche
package shapes

object Bernoulli {

  /** Creates an `automaton` resembling the Bernoulli distribution.
   *
   *  @param p the 'p' parameter of the returned Bernoulli distribution.
   */
  def apply(p: Double): DiscreteAutomaton[Boolean] = {
      require(p >= 0 && p <= 1)
    DiscreteAutomaton(Map(true -> p, false -> (1-p)))
  }
}
