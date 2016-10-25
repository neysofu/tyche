package io.neyofu.tyche
package util

object Density {

  /** Represents a right stochastic matrix.
   *
   *  Resorces
   *  --------
   *  1. https://en.wikipedia.org/wiki/Stochastic_matrix
   */
  class MassFunction[A](mf: Map[A, Seq[(Double, A)]]) {
    require(mf.values.forall { row =>
      row.map(_._1).sum == 1 && row.map(_._2).toSet == mf.keySet
    })
  }
}
