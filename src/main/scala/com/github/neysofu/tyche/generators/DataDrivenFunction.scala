package com.github.neysofu
package tyche
package generators

import scala.util.Random

case class DataDrivenFunction(points: Seq[Double]) extends (() => Double) {
  require(!points.isEmpty)

  protected val sortedPoints = points.sorted

  def apply: Double = {
    val d = Random.nextDouble * (points.size - 1)
    val downPoint = sortedPoints(d.floor.toInt)
    val upPoint = sortedPoints(d.ceil.toInt)
    (upPoint - downPoint) * (d % 1) + downPoint
  }
}
