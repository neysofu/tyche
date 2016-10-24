package io.neysofu.tyche
package stats

/** Represents a discrete probability distribution.
 *
 *  Resources
 *  ---------
 *  1. https://www.youtube.com/watch?v=3MOahpLxj6A
 */
class DiscreteDistribution[A](pmf: Seq[(Double, A)]) extends Distribution[A] { self =>
  
  private lazy val cumulativeSums = {
    var t: Double = 0
    pmf.map { s =>
      t += s._1
      t
    }
  }

  override def map[B](f: A => B) = {
    new DiscreteDistribution[B](pmf.map(x => (x._1, f(x._2))))
  }

  override def get = {
    val d = rnd.nextDouble
    pmf(cumulativeSums.indexWhere(_ > d))._2
  }
 }
