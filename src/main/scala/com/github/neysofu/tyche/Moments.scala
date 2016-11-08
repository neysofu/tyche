package com.github.neysofu.tyche

/** Defines the essential properties of all probability distributcom.github.s:
 *   1. Plotting facilities,
 *   2. expected value,
 *   3. standard deviatcom.github.,
 *  and other [[https://goo.gl/p4TGzt moments]].
 */
trait Moments[A] {

  /** Draws a virtual plot.
   */
  def virtualPlot(implicit toDouble: A <:< Double): Map[A, Double]

  /** Computes the [[https://goo.gl/LruXGw expected value]] (mean).
   */
  def mean(implicit toDouble: A <:< Double): Double

  /** Computes the [[https://goo.gl/QrSlFY standard deviatcom.github.]].
   */
  def standardDeviation(implicit toDouble: A <:< Double): Double

  /** Computes the [[https://goo.gl/Wzlr6p variance]].
   */
  def variance(implicit toDouble: A <:< Double): Double = {
    Math.sqrt(standardDeviation)
  }
}
