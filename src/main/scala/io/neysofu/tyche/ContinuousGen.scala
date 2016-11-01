package io.neysofu.tyche

/** Represents a continuous probability distribution with simple random
 *  sampling facilities.
 *  
 *  See:
 *   https://en.wikipedia.org/wiki/Simple_random_sample
 */
abstract class ContinuousGen[A] extends Gen[A] with Moments[A] {
  self =>

  val sampleSize: Int = 10000

  def plot(x: Int, y: Int)(implicit toDouble: A <:< Double): String = ""/*{
    val n = sampleSize + sampleSize % x
    val samples = Seq.fill(n)(toDouble(get)).sorted
    samples.grouped(x).toSeq.map(ls => "█" * ls.sum / ls.size)
  }*/

  def mean(implicit toDouble: A <:< Double) =
    toGenDouble.times(sampleSize).sum / sampleSize

  def standardDeviation(implicit toDouble: A <:< Double) = {
    val m = mean
    map(x => x*x - m).times(sampleSize).sum / sampleSize
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
  def chiSquare(k: Int): ContinuousGen[Double] = new ContinuousGen[Double] {
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
  def uniform: ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = random.nextDouble
  }
}
