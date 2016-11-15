package com.github.neysofu.tyche

/** A simple implementation trait for discrete random variables.
 */
trait DiscreteGen[A] extends Gen[A] with MassFunction[A] with Moments[A] {
    self =>

  def get: A = {
    val d = scala.util.Random.nextDouble
    outcomes(cdf indexWhere (_ > d))
  }

  def virtualPlot(implicit toDouble: A <:< Double): Map[A, Double] =
    (outcomes zip cdf).toMap

  def mean(implicit toDouble: A <:< Double): Double =
    (mass map {case (k, v) => toDouble(k) * v}).sum

  def standardDeviation(implicit toDouble: A <:< Double): Double = {
    val cacheMean = mean
    (mass map {case (k, v) => util.square(toDouble(k) - cacheMean) * v}).sum
  }
} 
