package io.neysofu.tyche
package stats

/** Provides fundamental numerical analysis methods through simple random
 *  sampling.
 *
 *  Resources
 *  ---------
 *  1. https://en.wikipedia.org/wiki/Simple_random_sample
 *  2. https://en.wikipedia.org/wiki/Numerical_analysis
 */
abstract class ContinuousGen[A] extends Gen[A] with Moments[A] {
  self =>

  /** The number of required observations.
   */
  val sampleSize: Int = 10000

  def plot(x: Int, y: Int)(implicit toDouble: A <:< Double): String = ""/*{
    val n = sampleSize + sampleSize % x
    val samples = Seq.fill(n)(toDouble(get)).sorted
    samples.grouped(x).toSeq.map(ls => "█" * ls.sum / ls.size)
  }*/

  def mean(implicit toDouble: A <:< Double) =
    toGenDouble.times(sampleSize).sum / sampleSize

  def stdDeviation(implicit toDouble: A <:< Double) = {
    def diff(x: Double, m: Double) = Math.pow(x, 2) - m
    map(diff(_, mean)).times(sampleSize).sum / sampleSize
  }
}

object ContinuousGen {
 
  /** Returns a Gaussian (normal) probability distribution.
   */
  def normal(sd: Double, eg: Double): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = random.nextGaussian * sd + eg
  }


  /** Returns a chi-squared distribution with ´k´ degrees of freedom.
   */
  def ChiSquare(k: Int): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = Seq.fill(k)(Math.pow(normal(1, 0).get, 2)).sum
  }

  /** Returns a binomial disribution with parameters ´n´ and ´p´.
   */
  def binomial(n: Int, p: Double): ContinuousGen[Int] = new ContinuousGen[Int] {
    def get = Seq.fill(n)(random.nextDouble).count(_ < p)
  }


  /** Returns a continuous uniform probability distribution in the interval
   *  ´[0;1[´.
   */
  def uniform(): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = random.nextDouble
  }

}
