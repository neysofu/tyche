package io.neysofu.tyche
package stats

object Commons {

  def newDiscreteUniform[A](seq: Seq[A]): DiscreteGen[A] = new DiscreteGen(
    DiscreteGen.probabilityMassFunct(seq.map(x => (1.0/seq.size, x)))
  )

  //def newMarkovChain[A](trans: Map[A, Seq[(Double, A)]]): DiscreteGen[DiscreteGen[A]]

  def newUniform(): ContinuousGen[Double] = {
    new ContinuousGen[Double] {
      def get = rnd.nextDouble
    }
  }
  
  def newGauss(sd: Double, eg: Double): ContinuousGen[Double] = {
    new ContinuousGen[Double] { self =>
      def get = rnd.nextGaussian * sd + eg
    }
  }

  def newStdNormal(): ContinuousGen[Double] = newGauss(1, 0)

  def newChiSquare(k: Int): ContinuousGen[Double] = {
    new ContinuousGen[Double] {
      def get = Seq.fill(k)(Math.pow(newStdNormal.get, 2)).sum
    }
  }

  def newBinomial(n: Int, p: Double): ContinuousGen[Int] = {
    new ContinuousGen[Int] {
      def get = Seq.fill(n)(rnd.nextDouble).count(_ < p)
    }
  }
}
