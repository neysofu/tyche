package io.neysofu.tyche

import scala.util.Random
import io.neysofu.tyche.util.Util

/** This class represents a discrete probability distribution characterized
 *  by a probability mass function.
 */
trait DiscreteGen[A] extends Gen[A] with MassFunction[A] with Moments[A] { self =>

  def get: A = {
    val d = Random.nextDouble
    outcomes(cdf.indexWhere(_ > d))
  }

  def mapPlot(implicit toDouble: A <:< Double): Map[A, Double] = {
    outcomes.zip(cdf).toMap
  }

  def mean(implicit toDouble: A <:< Double): Double = {
    val nth = 1 / mass.size
    mass.map { case (k, v) => toDouble(k) * v * nth }.sum
  }

  def standardDeviation(implicit toDouble: A <:< Double): Double = {
    val m = Util.square(mean) / mass.size
    mass.map { case (k, v) => Util.square(toDouble(k)) * v * m }.sum
  }
} 
