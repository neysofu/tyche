package io.neysofu.tyche

import io.neysofu.tyche.stats.Commons

object TestChild {

  def main(args: Array[String]) = {
    println(gToB)
  }

  sealed trait Child
  object M extends Child
  object F extends Child

  def family = Commons
    .newDiscreteUniform(Seq(M,F))
    .until(_.contains(M))
    .map(_.count(_ == F))

  val sampleSize = 10000
  val gToB = 1.0 * family.times(sampleSize).sum / sampleSize
}
