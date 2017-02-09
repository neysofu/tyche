package com.github.neysofu
package tyche
package shapes

import scala.util.Random
import util.Interval

case class Uniform(interval: Interval) extends ContinuousAutomaton {

  def apply: Double =
    Random.nextDouble * interval.difference + interval.from

  override def mean(implicit toDouble: Double => Double): Double =
    interval.sum / 2

  override def stdDeviation(implicit toDouble: Double => Double): Double =
    interval.difference / Math.sqrt(12)
}
