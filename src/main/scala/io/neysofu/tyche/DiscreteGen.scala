package io.neysofu.tyche

/** Represents a discrete probability distribution characterized by a
 *  probability mass function.
 */
abstract class DiscreteGen[A] extends Gen[A] {
  self =>

  /** The probability mass function; it contains all the possible outcomes and
   *  the respective weights. The weights are supposed to:
   *   1. be nonnegative, and
   *   2. sum up to 1.
   */
  val mass: Seq[(Double, A)]

  /** Returns a new discrete probability distribution originated from the
   *  current instance, the outcomes of which are changed accordingly
   *  to a bijective function.
   */
  def bijectiveMap[B](f: A => B): DiscreteGen[B] = new DiscreteGen[B] {
    val mass = self.mass.map(p => (p._1, f(p._2)))
  }

  def get: A = {
    val d = random.nextDouble
    mass(cdf.indexWhere(_ > d))._2
  }

  def plot(x: Int, y: Int)(implicit toDouble: A <:< Double): String = ""

  def mean(implicit toDouble: A <:< Double): Double =
    mass.map(p => p._1 * toDouble(p._2)).sum / mass.size

  def standardDeviation(implicit toDouble: A <:< Double): Double = {
    val m = mean
    mass.map(p => Math.pow(toDouble(p._2) * m, 2) * p._1).sum / mass.size
  }

  protected lazy val cdf = mass.map(_._1).scanLeft(0.0)(_+_).tail
} 
  
object DiscreteGen {

  def uniform[A](vs: A*): DiscreteGen[A] = new DiscreteGen[A] {
    val mass = vs.map(x => (1.0/vs.size, x))
  }

  def Bernoulli(p: Double): DiscreteGen[Boolean] = new DiscreteGen[Boolean] {
    val mass = Seq((p, true), (1-p, false))
  }
}
