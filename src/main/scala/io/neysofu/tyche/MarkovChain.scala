package io.neysofu.tyche

import scala.collection.mutable

trait MarkovChain[A] /*extends MassFunction[MarkovChain[A]]*/ { self =>

  val state: A
  protected val matrix: Map[A, Map[A, Double]]
  
  val mass = matrix(state)

  /*def train(seq: Seq[A])(p: Double = 0.5): Markovian[A] = new Markovian[A] {
    val nth = 1 / seq.size
    val pairs = seq.sliding(2)
    var table = matrix
    seq.sliding(2).foreach { case (k, v) =>
      
      table { case (k, v) =>
      val bonus = pairs.count(_ == Seq(k, v._2))
      k -> (k/2, v._2 + bonus * nth)
    }
  }*/
}

object MarkovChain {

  def emulate[A](seq: Seq[A]): MarkovChain[A] = new MarkovChain[A] {
    val state = seq.head
    val matrix = {
      val nth = 1.0 / seq.size
      var table = mutable.Map[A, mutable.Map[A, Double]]()
      for {
        pair <- seq.sliding(2).toList
        a = pair(0)
        b = pair(1)
      } yield {
        table(a)(b) = table(a)(b) + nth
      }
      table.map { case (k, v) =>
        k -> v.toMap
      }.toMap
    }
  }
}
