package io.neysofu.tyche
package util

object Minions {
  def cumulativeSum(seq: Seq[Double]): Seq[Double] = seq.scanLeft(0.0)(_+_)
}
