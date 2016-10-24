package io.neysofu.tyche
package stats

object Commons {

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

  def newChiSq(k: Int): ContinuousDistribution[Double] = {
    val gauss = newGauss(1, 0)
    new ContinuousDistribution[Double] {
      def get = Seq.fill(k)(Math.pow(gauss.get, 2)).sum
    }
  }

  def newBinomial(n: Int, p: Double): ContinuousDistribution[Int] = {
    new ContinuousDistribution[Int] {
      def get = Seq.fill(n)(rnd.nextDouble).count(_ < p)
    }
  }
}
