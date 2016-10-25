package io.neysofu.tyche
package stats

import io.neysofu.tyche.util.Minions

/** Represents a discrete probability distribution.
 *
 *  Resources
 *  ---------
 *  1. https://www.youtube.com/watch?v=3MOahpLxj6A
 */
class DiscreteDistribution[A](pmf: Seq[(Double, A)]) extends Distribution[A] { self =>
  
  private lazy val csum = Minions.cumulativeSum(pmf.map(_._1))

  override def map[B](f: A => B) = {
    new DiscreteDistribution[B](pmf.map(x => (x._1, f(x._2))))
  }

  override def get = {
    val d = rnd.nextDouble
    pmf(csum.indexWhere(_ > d))._2
  }
 }
