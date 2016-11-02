package io.neysofu.tyche

/** Represents a continuous ''PD'' with
  *   [[https://en.wikipedia.org/wiki/Simple_random_sample simple random sampling]]
  * facilities.
  */
trait ContinuousGen[A] extends Gen[A] with Sampling with Moments[A] { self =>

  def plot(implicit toDouble: A <:< Double) : String = {
    val sums = toGenDouble.times(sampleSize).sorted
      .grouped(sampleSize / 80).toList.map(seq => seq.sum/seq.size)
    val range = sums.head - sums.last
    val nth = 1 / sums.last
    util.PlotUtil.frameString {
      val columns = sums.map { d =>
        val height = Math.round(d * nth * 24).toInt
        ("#" * height).padTo(24, ' ').reverse
      }
      columns.transpose.map(_.mkString).mkString("\n")
    }
  }

  def mean(implicit toDouble: A <:< Double) = {
    toGenDouble.map(x => x * nthSampleSize).times(sampleSize).sum
  }

  def standardDeviation(implicit toDouble: A <:< Double) = {
    val m = mean
    map(x => (x*x-m) * nthSampleSize).times(sampleSize).sum
  }
}

object ContinuousGen {
 
  /** Returns a Gaussian (normal) distribution.
    */
  def normal(sd: Double, eg: Double): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = random.nextGaussian * sd + eg
  }

  /** Returns a chi-squared distribution.
    */
  def chiSquare(k: Int): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = Seq.fill(k)(Math.pow(normal(1, 0).get, 2)).sum
  }

  /** Returns a binomial distribution.
    */
  def binomial(n: Int, p: Double): ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = Seq.fill(n)(random.nextDouble).count(_ < p).toDouble
  }

  /** Returns a continuous uniform probability distribution in the interval
    * ´[0;1[´.
    */
  def uniform: ContinuousGen[Double] = new ContinuousGen[Double] {
    def get = random.nextDouble
  }
}
