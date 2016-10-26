package io.neysofu.tyche
package stats

/** Represents a discrete probability distribution.
 *
 *  Resources
 *  ---------
 *  1. https://www.youtube.com/watch?v=3MOahpLxj6A
 */
class DiscreteGen[A](val pmf: DiscreteGen.MassFunct[A]) extends Gen[A] { self =>

  override def map[B](f: A => B) = new DiscreteGen(
    new DiscreteGen.MassFunct(pmf.weights zip pmf.values.map(f(_)))
  )

  // See https://goo.gl/X4hwlu
  def get = {
    val d = rnd.nextDouble
    pmf.values(pmf.weights.indexWhere(_ > d))
  }
}

object DiscreteGen {

  class MassFunct[A](seq: Seq[(Double, A)]) {
    require(weights.last == 1)
    require(weights.forall(_ >= 0))

    def weights: Seq[Double] = seq.map(_._1)
    
    def values: Seq[A] = seq.map(_._2)
  }

  def probabilityMassFunct[A](seq: Seq[(Double, A)]): MassFunct[A] = {
    var t = 0.0
    new MassFunct(seq.map { dv =>
      t += dv._1
      (t, dv._2)
    })
  }
}
