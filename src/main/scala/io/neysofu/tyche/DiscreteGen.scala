package io.neysofu.tyche

/**
 * Represents a discrete probability distribution characterized by a
 * probability mass function (PMF).
 */
trait DiscreteGen[A] extends Gen[A] with ProbabilityMass[A] with Moments[A] { self =>

  /**
   * Returns a new discrete probability distribution originated from the
   * current instance, the outcomes of which are changed accordingly to a
   * bijective function.
   */
  def bijectiveMap[B](f: A => B): DiscreteGen[B] = new DiscreteGen[B] {
    val mass = self.mass.map(p => (p._1, f(p._2)))
  }

  def get: A = {
    val d = random.nextDouble
    mass(cdf.indexWhere(_ > d))._2
  }

  def plot(implicit toDouble: A <:< Double): String = {
    val sampleSize = 10000
    val sums = toGenDouble
      .times(sampleSize).sorted
      .grouped(sampleSize / 80)
      .toList
      .map(seq => seq.sum/seq.size)
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
  
object DiscreteGen {

  /**
   * Returns a discrete uniform distribution.
   */
  def uniform[A](vs: A*): DiscreteGen[A] = new DiscreteGen[A] {
    val mass = vs.map(x => (1.0/vs.size, x))
  }

  /**
   * Returns a Bernoulli distribution.
   */
  def Bernoulli(p: Double): DiscreteGen[Boolean] = new DiscreteGen[Boolean] {
    val mass = Seq((p, true), (1-p, false))
  }
}
