package com.github.neysofu
package tyche

/** Markovian processes are described as generators with a state and a
 *  transition rule.
 *
 *  @tparam A the type of the states.
 *  @see [[com.github.neysofu.tyche.Gen]], [[com.github.neysofu.tyche.DiscreteGen]]
 */
trait MarkovGen[A] extends Gen[MarkovGen[A]] {
    self =>

  /** The current state.
   */
  val state: A

  /** The transition rule. It takes a state as a parameter and returns the
   *  discrete random variable that will determine the next state.
   */
  val rule: A => DiscreteGen[A]
  
  /** Performs a random walk on the chain and stores the state at each step
   *  in a sequence.
   *
   *  @param n the length of the returned list.
   *  @return a new list of the given length `n` filled with successive
   *  states; an empty list if `n` is nonpositive.
   */
  def walk(n: Int): Seq[A] = if (n > 0) state +: apply.walk(n-1) else Seq()
 
  def apply = MarkovGen[A](self.rule, self.rule(self.state).apply)
}

/** Contains a number of builders for the most common Markov chains.
 */
object MarkovGen {

  def apply[A](f: A => DiscreteGen[A], current: A) = new MarkovGen[A] {
    val state = current
    val rule = f
  }

  /** Computes a unidimensional lattice path with steps ''+1'' and ''-1''.
   *
   *  @return a new Markovian process.
   */
  def randomWalk: MarkovGen[Int] = new MarkovGen[Int] {
    val state = 0
    val rule = x => new DiscreteGen[Int] {
      val mass = Map(x+1 -> 0.5, x-1 -> 0.5)
    }
  }

  /** Builds a new Markov chain modelled on some training data sequence.
   *
   *  @param seq the training data list.
   *  @return a new Markov chain with as many states as distinct elements
   *  in the given training data list `seq`. The initial state is set to the
   *  first element in the training data list.
   */
  def modelOn[A](seq: A*): MarkovGen[A] = {
      import scala.collection.mutable.{Map => Dict}
    require(
      seq.size >= 2, "The training list must contain at least two elements.")
    require(
      (seq indexWhere (_ == seq.last)) != seq.size - 1,
      "The training list provides no escape for its last element.")
    val table = Dict.empty[A, Dict[A, Double]]
    for (pair <- seq.sliding(2); a=pair(0); b=pair(1)) {
      if (!(table contains a)) {
        table(a) = Dict(b -> 1.0)
      } else if (!(table(a) contains b)) {
        table(a)(b) = 1.0
      } else {
        table(a)(b) += 1
      }
    }
    val ruleMap = table.map(kv => kv._1 -> kv._2.toMap).toMap
    new MarkovGen[A] {
      val state = seq.head
      val rule = (a: A) => new DiscreteGen[A] {
        val mass = ruleMap(a)
      }
    }
  }
}
