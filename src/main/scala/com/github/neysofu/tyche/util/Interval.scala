package com.github.neysofu
package tyche
package util

/** Represents an inclusive range of numbers.
 */
case class Interval(from: Double, to: Double) {
    require(from <= to)

  def difference: Double = to - from
  def sum: Double = from + to
  
  def contains(d: Double): Boolean = d >= from && d <= to
}

object Interval {
  val empty: Interval = Interval(0, 0)
  val decimalFraction: Interval = Interval(0, 1)
}
