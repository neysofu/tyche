package com.github.neysofu
package tyche

import scala.util.Random

/** Implements abstract methods for discrete distributions.
 */
trait DiscreteDistribution[A] extends Gen[A] {
    self =>

  override def toString = {
    val str = "empty discrete distribution"
    if (mass.size > 0) "non-" + str else str
  }

  override def filter(pred: A => Boolean): Gen[A] = {
    val filtered = DiscreteDistribution(mass filter (x => pred(x._1)))
    Gen(filtered())
  }

  /** The probability mass function. It contains all the possible values and
   *  their respective weights. The weights are supposed to be positive.
   */
  def mass: Map[A, Double]

  def replaceWith[B](f: A => B): DiscreteDistribution[B] =
    DiscreteDistribution(mass.map(kv => (f(kv._1) -> kv._2)))

  def apply: A = {
    val d = Random.nextDouble * weights.last
    values(weights indexWhere (_ > d))
  }

  override def probabilityOf(event: A => Boolean): Double =
    (mass filter (a => event(a._1))).values.sum / weights.last

  override def mean(implicit toDouble: A => Double): Double =
    (mass map {case (k, v) => toDouble(k) * v}).sum / weights.last

  override def stdDeviation(implicit toDouble: A => Double): Double = {
    val m = mean
    def diff(k: A) = Math.pow(toDouble(k)-m, 2)
    (mass map {case (k, v) => diff(k) * v}).sum / weights.last
  }

  protected lazy val values = mass.keys.toVector
  protected lazy val weights = mass.values.toVector.scanLeft(0.0)(_+_).tail
}

object DiscreteDistribution {

  def apply[A](pmf: Map[A, Double]): DiscreteDistribution[A] = new DiscreteDistribution[A] {
    val mass = pmf
  }
}
