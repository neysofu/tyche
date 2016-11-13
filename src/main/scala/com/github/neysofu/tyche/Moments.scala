package com.github.neysofu.tyche

/** Defines the essential properties of all probability distributions:
 *   1. Plotting facilities,
 *   2. expected value,
 *   3. standard deviation,
 *  and other [[https://goo.gl/p4TGzt moments]].
 */
trait Moments[A] {

  /** Draws a virtual plot.
   */
  def virtualPlot(implicit toDouble: A <:< Double): Map[A, Double]

  /** Computes the [[https://goo.gl/LruXGw expected value]] (mean).
   */
  def mean(implicit toDouble: A <:< Double): Double

  /** Computes the [[https://goo.gl/QrSlFY standard deviation]].
   */
  def standardDeviation(implicit toDouble: A <:< Double): Double

  /** Computes the [[https://goo.gl/Wzlr6p variance]].
   */
  def variance(implicit toDouble: A <:< Double): Double = {
    Math.sqrt(standardDeviation)
  }
}
