package com.github.neysofu.tyche

/** Markovian processes are described as generators with a state and a
 *  transitcom.github. rule.
 *
 *  @example {{{
 *  scala> val chain = new MarkovGen[Int] {
 *       |   val state = 0
 *       |   val rule = { (i: Int) =>
 *       |     DiscreteDistribution.uniform(Map(i+1 -> 0.5, i-1 -> 0.5))
 *       |   }
 *       | }
 *  chain: com.github.neysofu.tyche.MarkovGen[Int] = \$...
 *
 *  }}}
 *
 *  @see [[com.github.neysofu.tyche.Gen]], [[com.github.neysofu.tyche.DiscreteDistribution]]
 */
trait MarkovGen[A] extends Gen[MarkovGen[A]] { self =>

  /** The current state.
   */
  val state: A

  /** The transition rule. It takes a state as a parameter and returns the
   *  discrete random variable that will determine the next state.
   */
  val rule: A => DiscreteDistribution[A]

  /** Builds a new Markovian process by altering the state accordingly to
   *  the transition rule.
   */
  def get = new MarkovGen[A] {
    val state = self.rule(self.state).get
    val rule = self.rule
  }

  /** Performs a random walk and stores the state at each iteration in an
   *  array.
   */
  def walk(steps: Int): Seq[A] = steps match {
    case 0 => Seq()
    case _ => state +: get.walk(steps-1)
  }
}
