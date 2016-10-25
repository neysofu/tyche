package io.neysofu.tyche
package stats

object Commons {

  def newDiscreteUniform[A](seq: Seq[A]): DiscreteDistribution[A] = {
    val d = 1.0 / seq.size
    new DiscreteDistribution[A](seq.map((d, _)))
  }

  //def newMarkovChain[A](trans: Map[A, Seq[(Double, A)]]): DiscreteDistribution[DiscreteDistribution[A]]

  def newUniform(): ContinuousDistribution[Double] = {
    new ContinuousDistribution[Double] {
      def get = rnd.nextDouble
    }
  }
  
  def newGauss(sd: Double, eg: Double): ContinuousDistribution[Double] = {
    new ContinuousDistribution[Double] { self =>
      def get = rnd.nextGaussian * sd + eg
    }
  }

  def newStdNormal(): ContinuousDistribution[Double] = newGauss(1, 0)

  def newChiSquare(k: Int): ContinuousDistribution[Double] = {
    new ContinuousDistribution[Double] {
      def get = Seq.fill(k)(Math.pow(newStdNormal.get, 2)).sum
    }
  }

  def newBinomial(n: Int, p: Double): ContinuousDistribution[Int] = {
    new ContinuousDistribution[Int] {
      def get = Seq.fill(n)(rnd.nextDouble).count(_ < p)
    }
  }
}
