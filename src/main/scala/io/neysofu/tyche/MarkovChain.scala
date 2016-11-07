package io.neysofu.tyche

class MarkovChain[A](val rule: A => Map[A, Double], val state: A) extends MarkovGen[A]

object MarkovChain {

  def randomWalk: MarkovChain[Int] = new MarkovChain[Int](
    x => Map(x+1 -> 0.5, x-1 -> 0.5), 0
  )

  def estimateFrom[A](seq: A*): MarkovChain[A] = new MarkovChain[A](
    util.map2function({
      util.buildTable(seq).map { case (k, v) =>
        val total = v.values.sum
        k -> v.map { case (a, i) => a -> i.toDouble / total }
      }
    }),
    seq.head
  )
}
