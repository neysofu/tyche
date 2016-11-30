package com.github.neysofu
package tyche

import scala.util.Random

/** A trait for optimizing generator which use a hashtable as a generative
 *  function.
 */
trait DiscreteGen[A] extends Gen[A] {
    self =>

  /** The probability mass function. It contains all the possible values and
   *  their respective weights. The weights are supposed to be positive.
   */
  def mass: Map[A, Double]

  def apply: A = {
    val d = Random.nextDouble * weights.last
    values(weights indexWhere (_ > d))
  }

  override def toString = {
    val str = "empty discrete distribution"
    if (mass.size > 0) "non-" + str else str
  }

  override def filter(pred: A => Boolean): Gen[A] = {
    val filtered = DiscreteGen(mass filter (x => pred(x._1)))
    Gen(filtered())
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

object DiscreteGen {

  def apply[A](pmf: Map[A, Double]): DiscreteGen[A] = new DiscreteGen[A] {
    val mass = pmf
  }
}
