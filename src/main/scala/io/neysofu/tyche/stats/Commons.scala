package io.neysofu.tyche
package stats

/** A collection of useful probaiblity distribution generators.
 */
object Commons {

  /** Returns a discrete uniform probability distribution such that every
   *  one of the possible values has equal probability.
   *
   *  Resources
   *  ---------
   *  1. https://en.wikipedia.org/wiki/Discrete_uniform_distribution
   */
  def newDiscreteUniform[A](seq: Seq[A]): DiscreteGen[A] = new DiscreteGen(
    DiscreteGen.probabilityMassFunct(seq.map(x => (1.0/seq.size, x)))
  )

  /** Returns a Bernoulli probability distribution with Boolean outcomes.
   */
  def newBernoulli(p: Double): DiscreteGen[Boolean] = new DiscreteGen(
    DiscreteGen.probabilityMassFunct(Seq((1-p, false), (p, true)))
  )

  /** Returns a continuous uniform probability distribution in the interval
   *  ´[0;1[´.
   */
  def newUniform(): ContinuousGen[Double] = {
    new ContinuousGen[Double] {
      def get = rnd.nextDouble
    }
  }
 
  /** Returns a Gaussian probability distribution.
   */
  def newGauss(sd: Double, eg: Double): ContinuousGen[Double] = {
    new ContinuousGen[Double] { self =>
      def get = rnd.nextGaussian * sd + eg
    }
  }

  /** Returns a normal probability distribution.
   */
  def newStdNormal(): ContinuousGen[Double] = newGauss(1, 0)

  /** Returns a chi-squared distribution with ´k´ degrees of freedom.
   */
  def newChiSquare(k: Int): ContinuousGen[Double] = {
    new ContinuousGen[Double] {
      def get = Seq.fill(k)(Math.pow(newStdNormal.get, 2)).sum
    }
  }

  /** Returns a binomial disribution with parameters ´n´ and ´p´.
   */
  def newBinomial(n: Int, p: Double): ContinuousGen[Int] = {
    new ContinuousGen[Int] {
      def get = Seq.fill(n)(rnd.nextDouble).count(_ < p)
    }
  }
}
