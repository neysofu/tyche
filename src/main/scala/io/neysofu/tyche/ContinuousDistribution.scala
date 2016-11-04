package io.neysofu.tyche

/** This class represents a continuous probability distribution with
 *  [[https://goo.gl/5xB1A4 simple random sampling]] facilities.
 */
abstract class ContinuousDistribution[A] extends Gen[A] with Sampling with Moments[A] { self =>

  def plot(implicit toDouble: A <:< Double) : String = {
    val x = 80
    val y = 24
    
    val sums = toGenDouble
      .times(sampleSize)
      .sorted
      .grouped(sampleSize / x)
      .toList
      .map(seq => seq.sum / seq.size)
    val range = sums.head - sums.last
    val nth = 1 / sums.last
    util.PlotUtil.frameString(
      sums.map { d =>
        ("#" * Math.round(d * nth * y).toInt).padTo(y, ' ').reverse
      }.transpose.map(_.mkString).mkString("\n")
    )
  }

  def mean(implicit toDouble: A <:< Double) = {
    toGenDouble.map(x => x * nthSampleSize).times(sampleSize).sum
  }

  def standardDeviation(implicit toDouble: A <:< Double) = {
    val m = mean
    map(x => (x*x-m) * nthSampleSize).times(sampleSize).sum
  }
}

object ContinuousDistribution {
 
  /** Returns a normal distribution.
   *
   *  @param sd the standard deviation
   *  @param eg the expected value
   */
  def normal(sd: Double, eg: Double): ContinuousDistribution[Double] = new ContinuousDistribution[Double] {
    def get = random.nextGaussian * sd + eg
  }

  /** Returns a chi-squared distribution.
   *
   *  @param k the degrees of freedom
   */
  def chiSquare(k: Int): ContinuousDistribution[Double] = new ContinuousDistribution[Double] {
    def get = Seq.fill(k)(Math.pow(normal(1, 0).get, 2)).sum
  }

  /** Returns a binomial distribution.
   *
   *  @param n the number of independent yes/no experiments
   *  @param p the success probability
   */
  def binomial(n: Int, p: Double): ContinuousDistribution[Double] = new ContinuousDistribution[Double] {
    def get = Seq.fill(n)(random.nextDouble).count(_ < p).toDouble
  }

  /** Returns a uniform distribution in the interval `[0;1[`.
   */
  def uniform: ContinuousDistribution[Double] = new ContinuousDistribution[Double] {
    def get = random.nextDouble
  }
}
