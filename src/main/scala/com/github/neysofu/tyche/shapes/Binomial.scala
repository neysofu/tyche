package com.github.neysofu
package tyche
package shapes

import util.Memo
import scala.collection.mutable.{Map => Dict}

/**
 *
 *  @param n the number of independent yes/no experiments
 *  @param p the success probability
 */
case class Binomial(val n: Int, val p: Double) extends Automaton[Int] {

  private[this] val bernoulli = Bernoulli(p)

  override def apply: Int =
    Seq.fill(n)(if (bernoulli()) 1 else 0).sum

  override def mean(implicit toDouble: Int => Double): Double =
    n * p

  override def stdDeviation(implicit toDouble: Int => Double): Double =
    Math.sqrt(n * p * (1-p))

  lazy val mass: Map[Int, Double] = {
    val dict = Dict.empty[Int, Double]
    lazy val binCoefficient: Memo[(Int, Int), Long] = Memo {
      case (n, k) => if (k == 0 || k == n) {
        1L
      } else if (k > n/2) {
        binCoefficient(n, n-k)
      } else {
        binCoefficient(n-1, k-1) + binCoefficient(n-1, k)
      }
    }
    for (k <- 0 to n) {
      val pk = Math.pow(p, k)
      val qj = Math.pow(1-p, n-k)
      dict += (k -> binCoefficient(n, k) * pk * qj)
    }
    dict.toMap
  }
}
