package io.neysofu.tyche

/** This class represents a discrete probability distribution characterized
 *  by a probability mass function.
 */
abstract class DiscreteDistribution[A] extends Gen[A] with MassFunction[A] with Moments[A] { self =>

  /** Builds a new discrete probability distribution by applying a function
   *  to all the outcomes.
   */
  def bijectiveMap[B](f: A => B): DiscreteDistribution[B] = new DiscreteDistribution[B] {
    val mass = self.mass.map(p => (p._1, f(p._2)))
  }

  def get: A = {
    val d = random.nextDouble
    mass(cdf.indexWhere(_ > d))._2
  }

  def plot(implicit toDouble: A <:< Double): String = {
    val sampleSize = 10000
    val sums = toGenDouble.times(sampleSize).sorted.grouped(sampleSize / 80)
      .toList.map(seq => seq.sum/seq.size)
    val range = sums.head - sums.last
    val nth = 1 / sums.last
    util.PlotUtil.frameString(
      sums.map { d =>
        val height = Math.round(d * nth * 24).toInt
        ("#" * height).padTo(24, ' ').reverse
      }.transpose.map(_.mkString).mkString("\n")
    )
  }

  def mean(implicit toDouble: A <:< Double): Double = {
    val nth = 1 / mass.size
    mass.map(p => p._1 * toDouble(p._2) * nth).sum
  }

  def standardDeviation(implicit toDouble: A <:< Double): Double = {
    val m = mean
    val nth = 1 / mass.size
    mass.map(p => Math.pow(toDouble(p._2) * m, 2) * p._1 * nth).sum
  }
} 
  
object DiscreteDistribution {

  /** Returns a discrete uniform distribution.
   *
   *  @param outcomes the sample space
   */
  def uniform[A](outcomes: A*): DiscreteDistribution[A] = new DiscreteDistribution[A] {
    val w = 1.0/outcomes.size
    val mass = outcomes.map(x => (w, x))
  }

  /** Returns a Bernoulli distribution.
   *
   *  @param p the success probability
   */
  def Bernoulli(p: Double): DiscreteDistribution[Boolean] = new DiscreteDistribution[Boolean] {
    val mass = Seq((p, true), (1-p, false))
  }
}
