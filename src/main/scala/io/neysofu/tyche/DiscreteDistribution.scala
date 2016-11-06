package io.neysofu.tyche

case class DiscreteDistribution[A](val mass: Map[A, Double]) extends DiscreteGen[A] {
  require(mass.values.forall(_ >= 0))
  require(mass.values.sum == 1)
}

  
object DiscreteDistribution {

  /** Returns a discrete uniform distribution.
   *
   *  @param outcomes the sample space
   */
  def uniform[A](sampleSpace: A*): DiscreteDistribution[A] = {
    val w = 1.0 / sampleSpace.distinct.size
    new DiscreteDistribution[A](sampleSpace.distinct.map(x => x -> w).toMap)
  }

  /** Returns a Bernoulli distribution.
   *
   *  @param p the success probability
   */
  def Bernoulli(p: Double): DiscreteDistribution[Boolean] = {
    new DiscreteDistribution[Boolean](Map(true -> p, false -> (1-p)))
  }
}
