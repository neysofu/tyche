package com.github.neysofu.tyche

/** A wrapper class for [[com.github.neysofu.tyche.MarkovGen]].
 */
case class MarkovChain[A](rule: A => DiscreteDistribution[A], state: A) extends MarkovGen[A]

/** Contains a number of builders for the most common Markov chains.
 */
object MarkovChain {

  /** Builds a new Markovian process which generates a unidimensional
   *  [[https://en.wikipedia.org/wiki/Lattice_path lattice path]] with steps
   *  `+1` and `-1`.
   */
  def randomWalk: MarkovChain[Int] = new MarkovChain(
    x => DiscreteDistribution(Map(x+1 -> 0.5, x-1 -> 0.5)), 0
  )

  /** Builds a new Markov chain modelled on some training data sequence.
   *
   *  @param sequence the training data sequence
   *
   *  @example {{{
   *  scala> val anna = MarkovChain.modelOn("anna" : _*).walk(12)
   *  res1: Seq[Char] = List(a, n, a, n, n, a, n, a, n, a, n, n)
   *  
   *  }}}
   */
  def modelOn[A](sequence: A*): MarkovChain[A] = {
    require(
      sequence.size >= 2,
      "The training data sequence must contain at least two elements.")
    require(
      sequence.indexWhere(_ == sequence.last) != sequence.size - 1,
      "The training data sequence provides no escape for its last element")
    val mapRule = util.buildTable(sequence) map { case (k, v) =>
      val total = v.values.sum
      k -> DiscreteDistribution(v map { case (a, i) =>
        a -> i.toDouble / total
      })
    }
    new MarkovChain(util.map2function(mapRule), sequence.head)
  }
}
