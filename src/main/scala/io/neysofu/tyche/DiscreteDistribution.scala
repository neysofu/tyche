package io.neysofu.tyche

/** A wrapper class from [[io.neysofu.tyche.DiscreteGen]].
 */
case class DiscreteDistribution[A](val mass: Map[A, Double]) extends DiscreteGen[A] {
  require(
    mass.values.forall(_ >= 0),
    "All probabilities must be nonnegative.")
  require(
    mass.values.sum == 1,
    "The sums of probabilities must be equal to 1.")
}

  
object DiscreteDistribution {

  /** Returns a discrete uniform distribution.
   *  @param outcomes the sample space
   */
  def uniform[A](sampleSpace: A*): DiscreteDistribution[A] = {
    val w = 1.0 / sampleSpace.distinct.size
    DiscreteDistribution(sampleSpace.map(x => x -> w).toMap)
  }

  /** Returns a Bernoulli distribution.
   *  @param p the success probability
   */
  def Bernoulli(p: Double): DiscreteDistribution[Boolean] = {
    DiscreteDistribution(Map(true -> p, false -> (1-p)))
  }
}
