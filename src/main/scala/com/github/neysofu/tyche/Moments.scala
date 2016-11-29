package com.github.neysofu
package tyche

trait Moments[+A] {

  /** Computes the probability of an event.
   *
   *  @param p the event to analyse.
   *  @return the probability of the given event `event`.
   */
  def probabilityOf(p: A => Boolean): Double

  /** Computes the [[https://goo.gl/LruXGw expected value]] (mean).
   *
   *  @return the expected value.
   */
  def mean(implicit toDouble: A => Double): Double

  /** Computes the [[https://goo.gl/QrSlFY standard deviation]].
   *
   *  @return the standard deviation.
   */
  def standardDeviation(implicit toDouble: A => Double): Double
  
  /** Computes the [[https://goo.gl/Wzlr6p variance]].
   *
   *  @return the variance.
   */
  def variance(implicit toDouble: A => Double): Double =
    Math.sqrt(standardDeviation(toDouble))
}
